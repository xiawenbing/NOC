import chisel3._

import tree._
import crypto_ips._
import exampleIPs._

object Main extends App {
  // emitVerilog(new mod, Array("--target-dir", "generated"))
  emitVerilog(new Outter2(8, 4), Array("--target-dir", "generated"))
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
}