package interfaces.BOU

import chisel3._
import chisel3.util._

/*  The definition and implementation of
    basic operation unit (BOU) interface.
*/

// In BOUConfig, we define the behaviour of each type of BOU.
abstract class BOUConfig() {
  val data_width: Int // * 128-bit
  // inputs will be fed into each port, start from index 0, as much as possible
  val input_ports: Int
  val input_modes: Int
  val output_ports: Int
  val special_port_width: Int // 0 means no special port
  val setup_sequence: Seq[Int] // modes
  val work_sequence: Seq[Int]
}

class BOUInterfaceBundle(config: BOUConfig) extends Bundle {
  val inputs = Flipped(ValidIO(Vec(config.input_ports, Bits((128 * config.data_width).W))))
  val input_mode = Input(Bits(log2Ceil(config.input_modes).W))
  val outputs = Output(Vec(config.output_ports, Bits((128 * config.data_width).W)))
  val special = Input(Bits(config.special_port_width.W))
  val notice = Output(Bool())
  val notice_ack = Input(Bool())
}

class BOUModule(config: BOUConfig) extends Module {
  val io = IO(new BOUInterfaceBundle(config))
  val notice_reg = RegInit(false.B)

  when(io.notice_ack) {
    notice_reg := false.B
  }
  io.notice := notice_reg
}


/* Register-based design */

// abstract class BOUConfig() {
//   val registers_count: Int
//   val data_width: Int
//   val setup_sequence: Seq[Int]
//   val start_sequence: Seq[Int]
//   val end_sequence: Seq[Int]
//   // what about the control registers?
// }

// object MYBOUConfig extends BOUConfig {
//   val registers_count: Int = 9
//   val data_width: Int = 128
//   val setup_sequence: Seq[Int] = Seq(1, 2, 3)
//   val start_sequence: Seq[Int] = Seq(2)
//   val end_sequence: Seq[Int] = Seq(2)
// }

// data_width: The width of register data. For now we use 128-bit
// registers_count: The number of registers in slave module
// class BOUInterfaceBundle(data_width: Int, registers_count: Int) extends Bundle {
//   val wrAddr = Flipped(ValidIO(Bits(log2Ceil(registers_count).W)))
//   val wrData = Input(Bits(data_width.W))
//   val rdAddr = Flipped(ValidIO(Bits(log2Ceil(registers_count).W)))
//   val rdData = Output(Bits(data_width.W))
//   val notice = Output(Bool())
//   val notice_ack = Input(Bool())
// }

// class BOUModule(config: BOUConfig) extends Module {
//   val io = IO(new BOUInterfaceBundle(config.data_width, config.registers_count))
//   val reg_file = RegInit(VecInit.fill(config.registers_count)(0.U(config.data_width.W)))
//   val notice_reg = RegInit(false.B)

//   when(io.wrAddr.valid) {
//     reg_file(io.wrAddr.bits) := io.wrData
//   }
  
//   when(io.notice_ack) {
//     notice_reg := false.B
//   }

//   io.rdData := reg_file(io.rdAddr.bits)
//   io.notice := notice_reg
// }

// class myModule extends BOUModule(MYBOUConfig) {
//   import crypto_ips.MontgomeryMul
//   val mon = Module(new MontgomeryMul(256))
//   mon.io.din.bits.x := Cat(reg_file(1), reg_file(0))
//   mon.io.din.bits.y := Cat(reg_file(3), reg_file(2))
//   mon.io.din.bits.m := Cat(reg_file(5), reg_file(4))
//   mon.io.din.valid := reg_file(6)(0)
//   // we don't need ready
//   reg_file(7) := mon.io.dout.bits(127, 0)
//   reg_file(8) := mon.io.dout.bits(255, 128)
//   // we don't need valid, too
//   when(mon.io.dout.valid) {
//     notice_reg := true.B
//   }
// }
