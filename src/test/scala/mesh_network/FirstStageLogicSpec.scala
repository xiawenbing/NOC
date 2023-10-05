package mesh_network

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

class FirstStageLogicSpec extends AnyFreeSpec with ChiselScalatestTester {
  "FirstStage should select" in {
    test(new FirstStageLogic(1, 0)).withAnnotations(Seq(/* VerilatorBackendAnnotation,  */WriteVcdAnnotation)) { dut =>
      def pokeFlit(vc: Int, flit_type: FlitTypes.Type, vc_id: Int, data: String) = {
        dut.io.in_flits(vc).bits.header.flit_type.poke(flit_type)
        dut.io.in_flits(vc).bits.header.vc_id.poke(vc_id)
        dut.io.in_flits(vc).bits.load.poke(BigInt(data, 16))
        dut.io.in_flits(vc).valid.poke(true.B)
        // if(flit_type == FlitTypes.body || flit_type == FlitTypes.tail) {
        //   dut.clock.step()
        //   dut.io.in_flits(vc).valid.poke(false.B)
        // }
      }
      dut.clock.step(3)
      import FlitTypes._
      pokeFlit(1, body, 1, "DEAD")
      pokeFlit(0, body, 0, "4698")
      dut.clock.step(3)
      // dut.io.free_vc.bits.poke(0)
      // dut.io.free_vc.valid.poke(true)
      dut.io.winner_flit.ready.poke(true)
      // pokeFlit(single, 0, "BAD")
      // pokeFlit(body, 1, "ACDC")
      // pokeFlit(tail, 1, "FEBA")

      dut.clock.step(10)
    }
  }
}