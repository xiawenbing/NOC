package  interfaces.Router

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

/* Routers are organized in an m * n 2-D mesh. */

object NetworkSettings {
  val rows = 3
  val columns = 3
  val load_width = 96 // in bits
  val addr_width = 12 
  val data_length_width = 4
  val data_width = addr_width + data_length_width // data = (addr + length)
}

object PacketTypes extends ChiselEnum {
  val call = Value
  val empty = Value
}

class PacketHead extends Bundle {
  class coordinate extends Bundle {
    val x = UInt(log2Ceil(NetworkSettings.columns).W)
    val y = UInt(log2Ceil(NetworkSettings.rows).W)
  }
  val Packet_type = PacketTypes()
  val source = new coordinate
  val dest = new coordinate
}

abstract class PacketLoad extends Bundle

class Packet extends Bundle {
  val head = new PacketHead
  val load = Bits(NetworkSettings.load_width.W)
}

class CallPacket extends PacketLoad {
  class data extends Bundle {
    val addr = UInt(NetworkSettings.addr_width.W)
    val data_length = UInt(NetworkSettings.data_length_width.W)
  }
  val work_id = UInt(10.W)
  val inputs = UInt(3.W)
  val outputs = UInt(3.W)
  val all_data = Vec(5, new data)
  
}

class EmptyPacket extends PacketLoad {
  val ranodm = UInt(8.W)
  val tail = UInt((96 - 8).W)
}

class PacketDecoder extends Module {
  val io = IO(new Bundle {
    val pin = Input(new Packet())
    val call_Packet_out = Output(new CallPacket)
    val empty_pacakge_out = Output(new EmptyPacket)
  })

  io.call_Packet_out := 0.U.asTypeOf(new CallPacket)
  io.empty_pacakge_out := 0.U.asTypeOf(new EmptyPacket)

  import PacketTypes._
  switch(io.pin.head.Packet_type) {
    is(call) {
      val callP = io.pin.load.asTypeOf(new CallPacket)
      io.call_Packet_out := callP
    }
    is(empty) {
      val emptyP = io.pin.load.asTypeOf(new EmptyPacket)
      io.empty_pacakge_out := emptyP
    }
  }
}

class DecoderSTM extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val call_Packet_out = Output(new CallPacket)
    val empty_pacakge_out = Output(new EmptyPacket)
  })
  object state extends ChiselEnum {
    val idle = Value
    val send1 = Value
    val send2 = Value
  }
  val s = RegInit(state.idle)
  val d = Module(new PacketDecoder)

  d.io.pin := 0.U.asTypeOf(chiselTypeOf(d.io.pin))
  io.call_Packet_out := d.io.call_Packet_out
  io.empty_pacakge_out := d.io.empty_pacakge_out

  switch(s) {
    is(state.idle) {
      when(io.start) {
        s := state.send1
      }
    }
    is(state.send1) {
      d.io.pin.head.Packet_type := PacketTypes.empty
      // can't use BundleLiterals here
      val eP = Wire(new EmptyPacket)
      eP.ranodm := BigInt("24", 16).U
      eP.tail := BigInt("FADE00DEAD", 16).U
      d.io.pin.load := eP.asTypeOf(UInt(96.W))
      s := state.send2
    }
    is(state.send2) {
      d.io.pin.head.Packet_type := PacketTypes.call
      val cP = Wire(new CallPacket)
      cP.all_data.foreach{i => 
        i.addr := 0.U
        i.data_length := 0.U  
      }

      cP.work_id := BigInt("42", 16).U
      cP.inputs := 2.U
      cP.outputs := 1.U
      cP.all_data(0).addr := 11.U
      cP.all_data(0).data_length := 2.U
      cP.all_data(1).addr := 22.U
      cP.all_data(1).data_length := 1.U
      cP.all_data(2).addr := 33.U
      cP.all_data(2).data_length := 3.U
      d.io.pin.load := cP.asTypeOf(UInt(96.W))
      s := state.idle
    }
  }
}