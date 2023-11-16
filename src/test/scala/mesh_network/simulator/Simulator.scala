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
  type distance = Int

  class SimulationPara(val num_packets: Int,
                       val traffic_pattern: TrafficPattern) 

  class SimulationResults(val latency: List[(packetID, latency)],
                          val distance: List[(packetID, distance)],
                          val buffer_util: ArraySeq[Double],
                          val link_util: ArraySeq[ArraySeq[Double]])

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
    import Util._
    implicit val clk = d.clock
    val ports = d.io.locals
    val peeking_sigs = d.io.peeking_signals
    val injection_buffer: ArraySeq[List[soft_Flit]] = ArraySeq.fill(nodes)(Nil)
    val injection_vc: ArraySeq[Int] = ArraySeq.fill(nodes)(0) // paired with injection_buffer
    val buffer_util: ArraySeq[Int] = ArraySeq.fill(nodes)(0) // record used buffer
    val link_util: ArraySeq[ArraySeq[Int]] = ArraySeq.fill(nodes)(ArraySeq.fill(4)(0))
    var packets_to_inject = p.num_packets
    var packet_id = 0
    var injection_time_distance: List[(Int, Int, Int)] = Nil // (id, timestamp, distance), the distance is represented in hops
    var latency_per_hop: List[(Int, Int, Double)] = Nil // (id, latency, latency per hop)
    var overall_cycle: Int = 0

    val simu_log = new PrintWriter("simu-out/simu-log.txt")
    
    simu_log.println("*****************SIMULATION START*****************")
    // always ready to accept flits
    ports.foreach{port =>
      port.credit_in.foreach{credit =>
        credit.poke(buffer_depth.U)
      }
      port.flit_out.ready.poke(true)
    }

    while(latency_per_hop.length < p.num_packets) {
      simu_log.println(s"cycle: ${overall_cycle}")
      // check whether packets have been sent
      for(i <- 0 until nodes) {
        if(!injection_buffer(i).isEmpty) { // have more flit to send         
          // sent next flit if the router have enough rooms
          if(ports(i).credit_out(injection_vc(i)).peekInt() > 1) {
            pokeFlit(ports(i).flit_in, injection_buffer(i).head, injection_vc(i))
            ports(i).flit_in.valid.poke(true)
            injection_buffer(i) = injection_buffer(i).tail
          } else {
            ports(i).flit_in.valid.poke(false)
          }
        } else {
          ports(i).flit_in.valid.poke(false)
        }
      }
      // generate packets and place them into injection_buffer
      ports.zip(0 until nodes).foreach{ case(port, i) => 
        if(packets_to_inject != 0 &&
           injection_buffer(i).isEmpty &&
           !freeVCs(port).isEmpty &&
           !port.flit_in.valid.peekBoolean()) {
          val packet = p.traffic_pattern.genPacket(packet_id, idx2Coordinate(i))
          packet match {
            case Some(value) => {
              packets_to_inject = packets_to_inject - 1
              val flits = value.toFlits
              injection_buffer(i) = flits.tail
              injection_vc(i) = pickVC(freeVCs(port))
              // poke the first flit 
              pokeFlit(port.flit_in, flits.head, injection_vc(i))
              port.flit_in.valid.poke(true)
              injection_time_distance = (packet_id, overall_cycle, 
                                         Util.getDistance(flits.head.source, flits.head.dest)) :: injection_time_distance
              simu_log.println(s"\tPacket ID ${packet_id} generated with length ${flits.length} (from ${flits.head.source} to ${flits.head.dest}).")
              packet_id = packet_id + 1
            }
            case None => {}
          }
        }
      }
      
      for(i <- 0 until nodes) {
        // receive packets
        if(ports(i).flit_out.valid.peekBoolean()) {
          // suck a flit out in this cycle and record latency
          val f_type = ports(i).flit_out.bits.header.flit_type.peek()
          if(f_type == FlitTypes.head || f_type == FlitTypes.single) {
            val pid = extractPacketID(ports(i).flit_out.bits).toInt
            val record = injection_time_distance.find(_._1 == pid)
            record match {
              case None => throw new RuntimeException("Unrecorded packet!")
              case Some(value) => {
                val ltc = overall_cycle - value._2
                val ltc_per_hop = ltc.toDouble / value._3.toDouble
                simu_log.println(s"\tPacket ID ${pid} arrives ${idx2Coordinate(i)} with latency ${ltc}.")
                latency_per_hop = (pid, ltc, ltc_per_hop) :: latency_per_hop
              }
            }
          }
        }
        // record buffer utilization
        buffer_util(i) = buffer_util(i) + (buffer_depth * virtual_channels * 5 - peeking_sigs(i).free_buffers.peekInt().toInt)
        // record link utilization
        (0 until 4).zip(Seq(peeking_sigs(i).north_out_used,
                            peeking_sigs(i).south_out_used,
                            peeking_sigs(i).west_out_used,
                            peeking_sigs(i).east_out_used)).foreach{case(j, used) =>
          if(used.peekBoolean()) 
            link_util(i)(j) = link_util(i)(j) + 1
        }
      }
      // step forward
      d.clock.step()
      overall_cycle = overall_cycle + 1
    }
    simu_log.println("*****************SIMULATION DONE*****************")
    simu_log.println(s"cycle spent: ${overall_cycle}")
    val avg_latency = latency_per_hop.foldRight(0){case((_, ltc, _), sum) => ltc + sum}.toDouble / p.num_packets.toDouble
    simu_log.println(s"average latency per packet: ${fixedPrecisionDouble(avg_latency, 1)}")
    val avg_latency_per_hop = 
      latency_per_hop.foldRight(0.toDouble){case((_, _, ltc_per_hop), sum) => ltc_per_hop + sum} / p.num_packets.toDouble
    simu_log.println(s"average latency per hop per packet: ${fixedPrecisionDouble(avg_latency_per_hop, 1)}")
    simu_log.close()
    
    new SimulationResults(latency_per_hop.map{case(id, ltc, _) => (id, ltc)},
                          injection_time_distance.map{case(id, _, dist) => (id, dist)},
                          buffer_util.map(n => (n * 100).toDouble / 
                                               (buffer_depth * virtual_channels * 5 * overall_cycle).toDouble),
                          link_util.map(us => us.map(u => (u * 100).toDouble /
                                                          overall_cycle.toDouble)))
  }

  def recordResults(res: SimulationResults) = {
    import NetworkConfig._
    import Util._
    // record packet latency
    val latencyFile = new File("simu-out/latency.csv")
    val latencyWriter = new BufferedWriter(new FileWriter(latencyFile))
    latencyWriter.write("latency,count\n")
    
    def recordLatency(id_lat: List[(packetID, latency)], lat_count: List[(latency, count)]):
                      List[(latency, count)] = id_lat match {
      case Nil => lat_count
      case head :: next => {
        val op = lat_count.find(_._1 == head._2)
        op match {
          case None => recordLatency(next, (head._2, 1) :: lat_count)
          case Some(value) => recordLatency(next, lat_count.map{case (lat, count) =>
            if(lat == value._1) (lat, count + 1) else (lat, count)
          })
        }
      }
    }
    val lat_count = recordLatency(res.latency, Nil)
    lat_count.foreach{case(lat, count) => latencyWriter.write(s"${lat},${count}\n")}
    latencyWriter.close()
    
    // record packet latency and distance
    val latencyDistanceFile = new File("simu-out/latency_distance.csv")
    val latencyDistanceWriter = new BufferedWriter(new FileWriter(latencyDistanceFile))
    latencyDistanceWriter.write("latency,distance,count\n")

    val latencySorted = res.latency.sortBy(_._1)
    val distanceSorted = res.distance.sortBy(_._1)
    val latencyZipDistance = latencySorted.zip(distanceSorted).map{case((_, ltc), (_, dis)) => (ltc, dis)}

    def recordLatencyDistance(lat_dis: List[(latency, distance)], 
                              lat_dis_count: List[(latency, distance, count)])
                              : List[(latency, distance, count)] = lat_dis match {
      case Nil => lat_dis_count
      case head :: next => {
        val op = lat_dis_count.find{case(ltc, dis, _) => (ltc, dis) == head}
        op match {
          case None => recordLatencyDistance(next, (head._1, head._2, 1) :: lat_dis_count)
          case Some(value) => recordLatencyDistance(next, lat_dis_count.map{case(ltc, dis, count) =>
            if((ltc, dis) == head) (ltc, dis, count + 1) else (ltc, dis, count)
          })
        }
      }
    }
    val lat_dis_count = recordLatencyDistance(latencyZipDistance, Nil)
    lat_dis_count.foreach{case(lat, dis, count) =>
      latencyDistanceWriter.write(s"${lat},${dis},${count}\n")
    }
    latencyDistanceWriter.close()

    // record buffers utilization
    val buffersUtilFile = new File("simu-out/buffers_util.txt")
    val buffersUtilWriter = new BufferedWriter(new FileWriter(buffersUtilFile))
    res.buffer_util.foreach{util => 
      buffersUtilWriter.write(s"${fixedPrecisionDouble(util, 2)} ")
    }
    buffersUtilWriter.close()

    // record links utilization
    val linksUtilFile = new File("simu-out/links_util.csv")
    val linksUtilWriter = new BufferedWriter(new FileWriter(linksUtilFile))
    linksUtilWriter.write("router,direction,util\n")
    res.link_util.zip(0 until nodes).foreach{case(us, r) =>
      us.zip(Seq("North", "South", "West", "East")).foreach{case(u, direction) =>
        linksUtilWriter.write(s"${r},${direction},${fixedPrecisionDouble(u, 2)}\n")
      }
    }
    linksUtilWriter.close()
  }
}