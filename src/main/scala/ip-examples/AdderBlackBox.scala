package exampleIPs

import chisel3._
import chisel3.util.HasBlackBoxResource

class AdderBlackBox extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val a = Input(UInt(32.W))
    val b = Input(UInt(32.W))
    val clk = Input(Clock())
    val reset = Input(Reset())
    val sum = Output(UInt(32.W))
  })
  addResource("/adder.v")
}

class AdderTop extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(32.W))
    val in2 = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })
  val adder = Module(new AdderBlackBox)
  adder.io.clk := clock
  adder.io.reset := reset
  adder.io.a := io.in1
  adder.io.b := io.in2
  io.out := adder.io.sum
}