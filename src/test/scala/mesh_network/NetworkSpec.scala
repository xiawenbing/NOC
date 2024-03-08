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
      val traffic_pattern = new /* HotSpotTraffic(0.5, 8, List((0, 0), (3, 3))) */ TornatoTraffic(0.5, 8)
      val para = new Simulator.SimulationPara(500,
                      traffic_pattern)
      val res = Simulator.simulate(para)
      Simulator.recordResults(res)
      dut.clock.step(3)
    }
  }
}