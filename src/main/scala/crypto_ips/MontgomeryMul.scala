package crypto_ips

import chisel3._
import chisel3.util._

class MontgomeryMulInput(width: Int) extends Bundle {
  val x = UInt(width.W)
  val y = UInt(width.W)
  val m = UInt(width.W)
  val k = UInt(log2Ceil(width + 1).W)
}

class MontgomeryMul(width: Int) extends Module {
  val io = IO(new Bundle {
    val din = Flipped(DecoupledIO(new MontgomeryMulInput(width)))
    val dout = ValidIO(UInt(width.W))
  })

  val p = RegInit(0.U((width + 2).W))
  val a = RegInit(0.U((width + 2).W)) 
  val x = RegInit(0.U((width + 2).W))
  val y = RegInit(0.U((width + 2).W))
  val m = RegInit(0.U((width + 2).W))
  val k = RegInit(0.U(log2Ceil(width + 1).W)) // should be related to m
  val running = RegInit(false.B)
  val bitCount = RegInit(0.U(log2Ceil(width + 2).W))

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

import axinodes._

trait MonMulAXILiteSlave extends AXI4LiteSlaveInterface {
  // the axi library has a bug! don't create axi-lite interfaces that only has 2 or less registers!
  lazy val regCount = 8 * 4 + 3
  /* INPUT REG */
  lazy val x_reg        = Seq.fill(8)(RegInit(0.U(bitsWide.value.W)))
  lazy val y_reg        = Seq.fill(8)(RegInit(0.U(bitsWide.value.W)))
  lazy val m_reg        = Seq.fill(8)(RegInit(0.U(bitsWide.value.W)))
  lazy val control_reg  = RegInit(0.U(bitsWide.value.W))
  /* OUTPUT REG */
  lazy val res_reg      = Seq.fill(8)(RegInit(0.U(bitsWide.value.W)))
  lazy val status_reg   = RegInit(0.U(bitsWide.value.W))
  lazy val hello_reg    = RegInit(42.U(bitsWide.value.W))

  lazy val write_reg_map = (0 until 25).zip(x_reg ++ y_reg ++ m_reg :+ control_reg).map{case(i, r) => (i, AXI4LiteWriteReg(r))}
  lazy val read_reg_map = (25 until 35).zip(res_reg :+ status_reg :+ hello_reg).map{case(i, r) => (i, AXI4LiteReadReg(r))}

  lazy val regmap = AXI4LiteRegMap((write_reg_map ++ read_reg_map):_*)
  def connect_res_reg(in: UInt) = {
    // output_reg := in
    res_reg.zip(0 until 8).foreach{case(r, idx) =>
      r := in((idx + 1) * 32 - 1, idx * 32)
    }
  }
  def connect_status_reg(in: UInt) = {
    status_reg := in
  }
}

class MonMulAXI extends AXIModule {
  val aclock= IO(Input(Clock()))
  val areset= IO(Input(Bool()))
  withClockAndReset(aclock, !areset.asBool) {
    val MM = Module(new MontgomeryMul(256))
    val LiteSlave = new AXI4LiteSlaveNode with MonMulAXILiteSlave
    /* INPUT CONNECTION */
    MM.io.din.bits.x := LiteSlave.x_reg.reduceRight(Cat(_, _))
    MM.io.din.bits.y := LiteSlave.y_reg.reduceRight(Cat(_, _))
    MM.io.din.bits.m := LiteSlave.m_reg.reduceRight(Cat(_, _))
    MM.io.din.bits.k := LiteSlave.control_reg(8, 0)
    MM.io.din.valid := LiteSlave.control_reg(9)
    /* OUTPUT CONNECTION */
    when(MM.io.dout.valid) {
      LiteSlave.connect_res_reg(MM.io.dout.bits)
      LiteSlave.connect_status_reg(true.B)
    }
  }
}
