package exampleIPs

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class AdderBlackBoxTest extends AnyFreeSpec with ChiselScalatestTester {
  "adder should add" /* taggedAs RequiresVerilator */ in {
    test(new AdderTop).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.step(3)
      dut.io.in1.poke(3)
      dut.io.in2.poke(5)
      dut.clock.step()
      dut.io.out.expect(8)
      dut.io.in1.poke(2)
      dut.io.in2.poke(2)
      dut.clock.step(5)
      dut.io.out.expect(4)
    }
  }
}