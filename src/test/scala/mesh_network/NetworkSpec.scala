package mesh_network

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import helper._
import scala.collection.mutable.ArraySeq
import scala.util.Random
import java.io._

object Simulator {

  type latency = Int
  type packetID = Int
  type count = Int

  class SimulationPara(val num_packets: Int,
                       val injection_rate: Double) {
    require(injection_rate > 0 || injection_rate <= 1)
  }

  class SimulationResults(val latency: List[(Int, Int)])
  
  // may or may not generate a packet based on injection rate
  def genPacket(id: Int, source: (Int, Int), injection_rate: Double): Option[soft_Packet] = {
    require(injection_rate > 0 || injection_rate <= 1)
    import NetworkConfig._
    if(Random.nextInt(100) < injection_rate * 100) {
      var dest: (Int, Int) = (Random.nextInt(columns), Random.nextInt(rows))
      while(dest == source) {
        dest = (Random.nextInt(columns), Random.nextInt(rows))
      }
      Some(new TestPacket(id, source, dest, Random.nextInt(9) + 1))
    } else {
      None
    }
  }

  // peak the port and return a list of free VCs (only used when corresponding 
  // injection_buffer is empty)
  def freeVCs(p: RouterPort): List[Int] = {
    var res: List[Int] = Nil
    for(i <- 0 until NetworkConfig.virtual_channels) {
      if(p.credit_out(i).peekInt() == NetworkConfig.buffer_depth) {
        res = i :: res
      }
    }
    res
  }

  // randomly pick a VC from freeVC list 
  def pickVC(vcs: List[Int]): Int = {
    val idx = Random.nextInt(vcs.length)
    vcs(idx)
  }

  def extractPacketID(f: Flit): BigInt = {
    f.load.peekInt()
  }

  // simulate the network in a step-by-step manner
  // 
  def simulate(p: SimulationPara)(implicit d: NetworkExample): SimulationResults = {
    import NetworkConfig._
    implicit val clk = d.clock
    // TODO: change NetworkExample's io to a collection
    val ports: ArraySeq[RouterPort] = ArraySeq(d.io.local00, d.io.local01, d.io.local10, d.io.local11)
    val injection_buffer: ArraySeq[List[soft_Flit]] = ArraySeq.fill(nodes)(Nil)
    val injection_vc: ArraySeq[Int] = ArraySeq.fill(nodes)(0) // paired with injection_buffer
    var packets_to_inject = p.num_packets
    var packet_id = 0
    var injection_time: List[(Int, Int)] = Nil // (id, timestamp)
    var latency: List[(Int, Int)] = Nil // (id, latency)
    var overall_cycle: Int = 0

    val simu_log = new PrintWriter("simu-out/simu-log.txt")

    // always ready to accept flits
    ports.foreach{port =>
      port.credit_in.foreach{credit =>
        credit.poke(8.U)
      }
      port.flit_out.ready.poke(true)
    }

    while(latency.length < p.num_packets) {
      simu_log.println(s"cycle: ${overall_cycle}")
      // check whether packets have been sent
      for(i <- 0 until nodes) {
        if(ports(i).flit_in.ready.peekBoolean() &&
           ports(i).flit_in.valid.peekBoolean()) { // a flit has been sent in last cycle          
          // sent next flit
          if(!injection_buffer(i).isEmpty &&
             ports(i).credit_out(injection_vc(i)).peekInt() > 1) {
            pokeFlit(ports(i).flit_in, injection_buffer(i).head, injection_vc(i))
            ports(i).flit_in.valid.poke(true)
            injection_buffer(i) = injection_buffer(i).tail
          } else {
            ports(i).flit_in.valid.poke(false)
          }
        }
      }
      // generate packets and place them into injection_buffer
      ports.zip(0 until nodes).foreach{ case(port, i) => 
        val f_type = port.flit_in.bits.header.flit_type.peek()
        if(packets_to_inject != 0 &&
           injection_buffer(i).isEmpty &&
           !freeVCs(port).isEmpty &&
           !(port.flit_in.valid.peekBoolean() && (f_type == FlitTypes.tail || f_type == FlitTypes.single))) {
          val packet = genPacket(packet_id, idx2Coordinate(i), p.injection_rate)
          packet match {
            case Some(value) => {
              packets_to_inject = packets_to_inject - 1
              injection_time = (packet_id, overall_cycle) :: injection_time
              val flits = value.toFlits
              injection_buffer(i) = flits.tail
              injection_vc(i) = pickVC(freeVCs(port))
              // poke the first flit 
              pokeFlit(port.flit_in, flits.head, injection_vc(i))
              port.flit_in.valid.poke(true)
              simu_log.println(s"\tPacket ID ${packet_id} generated (from ${flits.head.source} to ${flits.head.dest}).")
              packet_id = packet_id + 1
            }
            case None => {}
          }
        }
      }
      // receive packets
      for(i <- 0 until nodes) {
        if(ports(i).flit_out.valid.peekBoolean()) {
          // suck a flit out in this cycle and record latency
          // println("~~~~~~~~~~!FLIT RECEIVED!~~~~~~~~~~~")
          val f_type = ports(i).flit_out.bits.header.flit_type.peek()
          if(f_type == FlitTypes.head || f_type == FlitTypes.single) {
            val pid = extractPacketID(ports(i).flit_out.bits).toInt
            val inj_time = injection_time.find(_._1 == pid)
            inj_time match {
              case None => throw new RuntimeException("Unrecorded packet!")
              case Some(value) => {
                val ltc = overall_cycle - value._2
                simu_log.println(s"\tPacket ID ${pid} arrives with latency ${ltc}.")
                latency = (pid, ltc) :: latency
              }
            }
          }
        }
      }
      // println(s"latency.length: ${latency.length}")

      // step forward
      d.clock.step()
      overall_cycle = overall_cycle + 1
    }
    simu_log.println("*****************SIMULATION DONE*****************")
    simu_log.println(s"cycle spent: ${overall_cycle}")
    simu_log.close()
    
    new SimulationResults(latency)
  }

  def recordResults(res: SimulationResults) = {
    // record packet latency
    val latencyFile = new File("simu-out/latency.csv")
    val latencyWriter = new BufferedWriter(new FileWriter(latencyFile))
    latencyWriter.write("latency,count\n")
    
    def recordLatency(id_lat: List[(packetID, latency)], lat_count: List[(latency, count)]):
                      List[(latency, count)] = {
      id_lat match {
        case Nil => lat_count
        case head :: next => {
          val op = lat_count.find(_._1 == head._2)
          op match {
            case None => recordLatency(next, (head._2, 1) :: lat_count)
            case Some(value) => recordLatency(next, lat_count.map{case (lat, count) =>
              if(lat == head._2) (lat, count + 1) else (lat, count)
            })
          }
        }
      }
    }
    val lat_count = recordLatency(res.latency, Nil)
    lat_count.foreach{case(lat, count) => latencyWriter.write(s"${lat},${count}\n")}
    latencyWriter.close()
  }
}

class NetworkSpec extends AnyFreeSpec with ChiselScalatestTester {
  "simulator" in {
    test(new NetworkExample).withAnnotations(Seq(VerilatorBackendAnnotation/* , WriteVcdAnnotation */)) { dut =>
      implicit val clk = dut.clock
      implicit val d = dut
      
      dut.clock.step(3)
      // dut.io.local00.credit_in(0).poke(8)
      // dut.io.local00.credit_in(1).poke(8)
      // dut.io.local00.flit_out.ready.poke(true)
      // dut.io.local01.credit_in(0).poke(8)
      // dut.io.local01.credit_in(1).poke(8)
      // dut.io.local01.flit_out.ready.poke(true)
      // val pkt = new TestPacket(1, (1, 0), (0, 1), 2, Array(BigInt("F10D01A", 16), BigInt("F10D01B", 16)))
      // var flits = pkt.toFlits
      // while(!flits.isEmpty) {
      //   val flit = flits.head
      //   dut.io.local10.flit_in.valid.poke(true.B)
      //   pokeFlit(dut.io.local10.flit_in, flit, 1)
      //   flits = flits.tail
      // }
      // dut.clock.step(20)
      val para = new Simulator.SimulationPara(20, 0.5)
      val res = Simulator.simulate(para)
      Simulator.recordResults(res)
      dut.clock.step(3)
    }
  }
}