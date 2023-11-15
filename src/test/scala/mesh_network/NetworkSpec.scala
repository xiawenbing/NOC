package mesh_network

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import helper._
import scala.collection.mutable.ArraySeq
import scala.util.Random
import java.io._

class NetworkSpec extends AnyFreeSpec with ChiselScalatestTester {
  "simulator" in {
    test(new NetworkExample(true)).withAnnotations(Seq(VerilatorBackendAnnotation , WriteVcdAnnotation)) { dut =>
      implicit val clk = dut.clock
      implicit val d = dut
      
      dut.clock.step(3)
      val para = new Simulator.SimulationPara(20,
                      new HotSpotTraffic(0.5, 8, (0, 0)))
      val res = Simulator.simulate(para)
      Simulator.recordResults(res)
      dut.clock.step(3)
    }
  }
}