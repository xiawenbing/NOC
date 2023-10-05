package Tesla

import chisel3._
import chisel3.util._

class share_memory (width:Int,depth:Int) extends Module {
    val io = IO(new Bundle {
    val rdAddr = Input(UInt (log2Ceil(depth).W))
    val rdData = Output (UInt (width.W))
    val wrAddr = Input(UInt (log2Ceil(depth).W))
    val wrData = Input(UInt (width.W))
    val wrEna = Input(Bool ())
    })
    val mem = SyncReadMem (width , UInt (depth.W))
    io. rdData := mem.read(io. rdAddr )
    when(io. wrEna) {
    mem. write (io.wrAddr , io. wrData )
    }
}