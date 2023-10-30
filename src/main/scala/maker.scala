import interfaces.BOU._
import chisel3._

// import tree._
import crypto_ips._
import exampleIPs._
import interfaces._
import mesh_network._

object Main extends App {
  // emitVerilog(new mod, Array("--target-dir", "generated"))
  // emitVerilog(new myModule, Array("--target-dir", "generated"))
  // emitVerilog(new Router(32, 2, 4), Array("--target-dir", "generated"))
  // emitVerilog(new tree4, Array("--target-dir", "generated"))
  // emitVerilog(new tree16, Array("--target-dir", "generated"))
  // emitVerilog(new tree32, Array("--target-dir", "generated"))
  // emitVerilog(new tree64, Array("--target-dir", "generated"))
  // emitVerilog(new tree128, Array("--target-dir", "generated"))
  // emitVerilog(new SBoxAXI, Array("--target-dir", "generated"))
  // emitVerilog(new AdderTop, Array("--target-dir", "generated"))
  // emitVerilog(new MontgomeryMul(256), Array("--target-dir", "generated"))
  // emitVerilog(new Counter(4), Array("--target-dir", "generated"))
  // emitVerilog(new BOUMonMul, Array("--target-dir", "generated"))
  // emitVerilog(new DecoderSTM, Array("--target-dir", "generated"))
  emitVerilog(new Granter(5), Array("--target-dir", "generated"))
  // emitVerilog(new InputBuffers, Array("--target-dir", "generated"))
  // emitVerilog(new VirtualChannelQ, Array("--target-dir", "generated"))
  // emitVerilog(new Router(1, 1), Array("--target-dir", "generated"))
  // emitVerilog(new NetworkExample, Array("--target-dir", "generated"))
}