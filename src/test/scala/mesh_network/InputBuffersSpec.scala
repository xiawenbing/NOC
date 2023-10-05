package mesh_network

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class InputBuffersSpec extends AnyFreeSpec with ChiselScalatestTester {
  "InputBuffers should buffer" in {
    test(new InputBuffers).withAnnotations(Seq(/* VerilatorBackendAnnotation,  */WriteVcdAnnotation)) { dut =>
      def pokeFlit(flit_type: FlitTypes.Type, vc_id: Int, data: String) = {
        dut.io.in_flit.bits.header.flit_type.poke(flit_type)
        dut.io.in_flit.bits.header.vc_id.poke(vc_id)
        dut.io.in_flit.bits.load.poke(BigInt(data, 16))
        dut.io.in_flit.valid.poke(true.B)
        dut.clock.step()
        dut.io.in_flit.valid.poke(false.B)
      }
      dut.clock.step(3)
      import FlitTypes._
      pokeFlit(head, 1, "DEAD")
      pokeFlit(single, 0, "BAD")
      pokeFlit(body, 1, "ACDC")
      pokeFlit(tail, 1, "FEBA")
      dut.clock.step(3)
      dut.io.out_flits(0).ready.poke(true)
      dut.io.out_flits(1).ready.poke(true)
      dut.clock.step(10)
    }
  }
}