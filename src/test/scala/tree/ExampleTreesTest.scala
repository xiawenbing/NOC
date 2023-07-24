package tree

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._

class ExampleTreesTest extends AnyFreeSpec with ChiselScalatestTester {
  "sboxes should pass" in {
    test(new tree16).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(3)
      dut.io.input.bits.data.poke(0x00010203)
      dut.io.input.bits.addr.poke(11)
      dut.io.input.valid.poke(true)
      dut.clock.step()
      dut.io.input.bits.data.poke(0x01bbccdd)
      dut.io.input.bits.addr.poke(6)
      dut.io.input.valid.poke(true)
      dut.clock.step()
      dut.io.input.bits.data.poke(0x02223344)
      dut.io.input.bits.addr.poke(3)
      dut.io.input.valid.poke(true)
      dut.clock.step()
      dut.io.input.bits.data.poke(0x03667788)
      dut.io.input.bits.addr.poke(15)
      dut.io.input.valid.poke(true)
      dut.clock.step()
      dut.io.input.valid.poke(false)
      dut.clock.step(6)
    }
  }
}