package interfaces.BOU

import chisel3._
import chisel3.util._

/* Example of using the BOU interface */

/* 1. Extending BOUConfig with your own configuration. */
object MonMulConfig extends BOUConfig {
  val data_width: Int = 2
  val input_ports: Int = 2
  val input_modes: Int = 2
  val output_ports: Int = 1

  val special_port_width: Int = log2Ceil(data_width * 128 + 1)
  val setup_sequence: Seq[Int] = Seq(1)
  val work_sequence: Seq[Int] = Seq(0)
}

/* 2. Define a BOUModule (extending BOUModule with the configuration you created). */
/* 3. In the BOUModule, initialize your own design and connect it with the interfaces.*/
class BOUMonMul extends BOUModule(MonMulConfig) {
  import crypto_ips.MontgomeryMul
  val mon = Module(new MontgomeryMul(256))

  val m_reg = RegInit(0.U(256.W))
  val output_reg = RegInit(0.U(256.W)) // only need this if the IP can't hold output by itself

  mon.io.din.bits.m := m_reg
  mon.io.din.bits.x := DontCare
  mon.io.din.bits.y := DontCare
  mon.io.din.bits.k := DontCare
  mon.io.din.valid := false.B

  when(io.input_mode === 1.U && io.inputs.fire) {
    m_reg := io.inputs.bits(0)
  }

  when(io.input_mode === 0.U && io.inputs.fire) {
    mon.io.din.bits.x := io.inputs.bits(0)
    mon.io.din.bits.y := io.inputs.bits(1)
    mon.io.din.bits.k := io.special
    mon.io.din.valid := true.B
  }

  when(mon.io.dout.fire) {
    output_reg := mon.io.dout.bits
    notice_reg := true.B
  }
  io.outputs(0) := output_reg
}