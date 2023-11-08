package mesh_network

import chisel3._
import chisel3.util._

class SchedulerPort extends Bundle {
  import NetworkConfig._
  val flits_out = Vec(virtual_channels, Decoupled(new Flit))
  // the scheduler should form the flit, though leaving some field empty,
  // such as virtual channels, src and dest.
  // assumes that a packet will come in order
  val flit_in = Flipped(Decoupled(new Flit))
}

//  ------       ----       -----------
// |router| <-> | NI | <-> | scheduler |
//  ------       ----       ----------- 
// achieves routing configurations here
// for packets sending out from the scheduler, this module assigns a
// virtual channel for it.
class NetworkInterface(x: Int, y: Int) extends Module {
  import NetworkConfig._
  import SystemConfig._
  val io = IO(new Bundle {
    val router_port = new RouterPort
    val scheduler_port = new SchedulerPort
  })

  object sendingStates extends ChiselEnum {
    val idle = Value
    val remain = Value
  }
  import sendingStates._

  val inputBuffer = Module(new InputBuffers)
  // for now we only support unicast
  // TODO: a task may use an IP more than once...
  val nextDestReg = RegInit(VecInit.fill(max_tasks)(0.U.asTypeOf(new Coordinate)))
  val vcQueue = Module(new VirtualChannelQ)
  val sendingSTM = RegInit(idle)
  val vcReg = RegInit(0.U(log2Ceil(virtual_channels).W))
  val tailJustLeave = RegInit(VecInit.fill(virtual_channels)(false.B))

  io.router_port.flit_in <> inputBuffer.io.in_flit
  io.router_port.credit_out := inputBuffer.io.room
  io.scheduler_port.flits_out <> inputBuffer.io.out_flits
  io.router_port.flit_out.valid := false.B
  io.router_port.flit_out.bits := 0.U.asTypeOf(new Flit)
  vcQueue.io.enq.valid := false.B
  vcQueue.io.enq.bits := DontCare
  vcQueue.io.deq.ready := false.B

  inputBuffer.io.out_flits.zip(io.scheduler_port.flits_out)foreach{case(in, out) => 
    when(in.valid) {
      when(in.bits.header.flit_type === FlitTypes.single) {
        val headFlitLoad = in.bits.load.asTypeOf(new HeadFlitLoad)
        when(PacketTypes.safe(headFlitLoad.packet_type)._1 === PacketTypes.packet_route) {
          // capture route config packet
          out.valid := false.B
          in.ready := true.B
          val config_info = headFlitLoad.data.asTypeOf(new RouteConfig)
          nextDestReg(headFlitLoad.task_id) := config_info.next_dest
        }
      }
    }
  }

  io.scheduler_port.flit_in.ready := false.B
  switch(sendingSTM) {
    is(idle) {
      io.scheduler_port.flit_in.ready := vcQueue.io.deq.valid && 
                                         io.router_port.credit_in(vcQueue.io.deq.bits) > 1.U
      when(io.scheduler_port.flit_in.fire) {
        val headFlitLoad = Wire(new HeadFlitLoad)
        headFlitLoad := io.scheduler_port.flit_in.bits.load.asTypeOf(new HeadFlitLoad)
        headFlitLoad.source.x := x.U
        headFlitLoad.source.y := y.U
        headFlitLoad.dest.x := nextDestReg(headFlitLoad.task_id).x
        headFlitLoad.dest.y := nextDestReg(headFlitLoad.task_id).y

        val flitHeader = Wire(new FlitHeader)
        flitHeader.flit_type := io.scheduler_port.flit_in.bits.header.flit_type
        flitHeader.vc_id := vcQueue.io.deq.bits
        vcQueue.io.deq.ready := true.B
        vcReg := vcQueue.io.deq.bits
        
        io.router_port.flit_out.valid := true.B
        io.router_port.flit_out.bits.header := flitHeader
        io.router_port.flit_out.bits.load := headFlitLoad.asTypeOf(Bits(flit_load_width.W))
        when(io.scheduler_port.flit_in.bits.header.flit_type === FlitTypes.head) {
          sendingSTM := remain
        }.otherwise { // single
          tailJustLeave(vcQueue.io.deq.bits) := true.B
        }
      }
    }
    is(remain) {
      io.scheduler_port.flit_in.ready := io.router_port.credit_in(vcReg) > 1.U
      when(io.scheduler_port.flit_in.fire) {
        io.router_port.flit_out.valid := true.B
        io.router_port.flit_out.bits := io.scheduler_port.flit_in.bits
        when(io.scheduler_port.flit_in.bits.header.flit_type === FlitTypes.tail) {
          sendingSTM := idle
          tailJustLeave(vcReg) := true.B
        }
      }
    }
  }
  
  io.router_port.credit_in.zip(tailJustLeave).zip(0 until virtual_channels).foreach{case((credit, l), v) =>
    when(credit === buffer_depth.U && l) {
      vcQueue.io.enq.valid := true.B
      vcQueue.io.enq.bits := v.U
      l := false.B
    }
  }
}