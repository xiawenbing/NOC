package mesh_network

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class NetworkInterfaceSpec extends AnyFreeSpec with ChiselScalatestTester {
  "NetworkInterface" in {
    test(new NetworkInterface(1, 1)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
        port.valid.poke(true)
        helper.pokeFlit(port, flit, vc_id)
        dut.clock.step()
        port.valid.poke(false)
      }
      import helper._
      implicit val clk = dut.clock
      implicit val d = dut

      dut.io.router_port.credit_in.foreach{p => p.poke(8)}
      dut.io.router_port.flit_out.ready.poke(true)

      dut.clock.step(3)
      val pkt1 = (new soft_RouteConfigPacket((0, 0), (1, 1), 5, (1, 0))).toFlits
      enqFlit(dut.io.router_port.flit_in, pkt1.head, 1)
      dut.clock.step(3)
      val pkt2 = (new soft_RouteConfigPacket((0, 0), (1, 1), 8, (0, 1))).toFlits
      enqFlit(dut.io.router_port.flit_in, pkt2.head, 0)
      dut.clock.step(3)
      val pkt3 = (new soft_DataPacket(5, BigInt("DEAD", 16))).toFlits
      enqFlit(dut.io.scheduler_port.flit_in, pkt3.head, 0)
      dut.clock.step(3)
      val pkt4 = (new soft_DataPacket(8, BigInt("BED", 16))).toFlits
      enqFlit(dut.io.scheduler_port.flit_in, pkt4.head, 0)
      dut.clock.step(10)
    }
  }
}

class NetworkWithNISpec extends AnyFreeSpec with ChiselScalatestTester {
  "NetworkWithNI" in {
    test(new NetworkExampleWithNI).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
        port.valid.poke(true)
        helper.pokeFlit(port, flit, vc_id)
        dut.clock.step()
        port.valid.poke(false)
      }
      import helper._
      implicit val clk = dut.clock
      implicit val d = dut

      dut.io.local01.flits_out.foreach(p => p.ready.poke(true))
      dut.io.local10.flits_out.foreach(p => p.ready.poke(true))
      dut.io.local11.flits_out.foreach(p => p.ready.poke(true))

      dut.clock.step(3)
      var pkt = (new soft_RouteConfigPacket((0, 0), (1, 1), 5, (1, 0))).toFlits
      enqFlit(dut.io.local00.flit_in, pkt.head, 1)
      dut.clock.step()
      pkt = (new soft_RouteConfigPacket((0, 0), (0, 1), 9, (1, 0))).toFlits
      enqFlit(dut.io.local00.flit_in, pkt.head, 0)
      dut.clock.step(10)

      pkt = (new soft_DataPacket(5, BigInt("F11D10", 16))).toFlits
      enqFlit(dut.io.local11.flit_in, pkt.head, 0)
      pkt = (new soft_DataPacket(9, BigInt("F01D10", 16))).toFlits
      enqFlit(dut.io.local01.flit_in, pkt.head, 0)

      dut.clock.step(10)
    }
  }
}