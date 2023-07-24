package tree

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class RouterTest extends AnyFreeSpec with ChiselScalatestTester {
  "Waveform should pass" in {
    test(new OutputRouter(32, 2, 4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(3)
      dut.io.input(2).valid.poke(true)
      dut.io.input(2).bits.addr.poke(1)
      dut.io.input(2).bits.data.poke(8)
      dut.clock.step(3)
      dut.io.input(2).valid.poke(false)
      dut.io.input(3).valid.poke(true)
      dut.io.input(3).bits.addr.poke(2)
      dut.io.input(3).bits.data.poke(5)
      dut.clock.step(3)
      dut.io.input(3).valid.poke(false)
      dut.clock.step(3)
    }
  }
}