package interfaces.Router

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class DecoderSpec extends AnyFreeSpec with ChiselScalatestTester {
  "Decoder should decode package" /* taggedAs RequiresVerilator */ in {
    test(new DecoderSTM).withAnnotations(Seq(/* VerilatorBackendAnnotation,  */WriteVcdAnnotation)) { dut =>
      dut.clock.step(3)
      // dut.io.pin.head.package_type.poke(PackageTypes.empty)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.empty_pacakge_out)).ranodm.poke(42)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.empty_pacakge_out)).tail.poke(BigInt("FADE00DEAD", 16))
      // dut.io.pin.load.poke(BigInt("FADE00DEAD", 16))
      // dut.clock.step(3)
      // dut.io.pin.head.package_type.poke(PackageTypes.call)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).work_id.poke(666)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).inputs.poke(2)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).outputs.poke(1)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).all_data(0).addr.poke(33)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).all_data(0).data_length.poke(2)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).all_data(1).addr.poke(55)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).all_data(1).data_length.poke(1)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).all_data(2).addr.poke(44)
      // // dut.io.pin.load.asTypeOf(chiselTypeOf(dut.io.call_package_out)).all_data(2).data_length.poke(3)
      // dut.io.pin.load.poke(BigInt("ABED11FEED", 16))
      dut.io.start.poke(true)
      dut.clock.step(5)
    }
  }
}