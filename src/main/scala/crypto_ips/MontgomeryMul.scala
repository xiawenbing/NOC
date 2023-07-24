package crypto_ips

import chisel3._
import chisel3.util._

class MontgomeryMulInput(width: Int) extends Bundle {
  val x = UInt(width.W)
  val y = UInt(width.W)
  val m = UInt(width.W)
  val k = UInt(log2Ceil(width).W)
}

class MontgomeryMul(width: Int) extends Module {
  val io = IO(new Bundle {
    val din = Flipped(DecoupledIO(new MontgomeryMulInput(width)))
    val dout = ValidIO(UInt(width.W))
  })

  val p = RegInit(0.U(width.W))
  val a = RegInit(0.U(width.W)) 
  val x = RegInit(0.U(width.W))
  val y = RegInit(0.U(width.W))
  val m = RegInit(0.U(width.W))
  val k = RegInit(0.U(log2Ceil(width).W))
  val running = RegInit(false.B)
  val bitCount = RegInit(0.U(log2Ceil(width).W))

  io.din.ready := !running
  when(io.din.fire) {
    running := true.B
    x := io.din.bits.x
    y := io.din.bits.y
    m := io.din.bits.m
    k := io.din.bits.k
  }
  when(running) {
    val new_a = p + Mux(x(bitCount), y, 0.U)
    a := new_a
    p := Mux(new_a(0), (new_a + m) >> 1.U, new_a >> 1.U)
    bitCount := bitCount + 1.U
  }
  when(running && bitCount === k) {
    bitCount := 0.U
    p := 0.U
    io.dout.bits := Mux(p >= m, p - m, p)
    io.dout.valid := true.B
    running := false.B
  }.otherwise {
    io.dout.bits := DontCare
    io.dout.valid := false.B
  }
}

// class ModMutiplier(width: Int) extends Module {

// }