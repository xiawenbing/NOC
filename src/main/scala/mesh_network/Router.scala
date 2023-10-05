package mesh_network

import chisel3._
import chisel3.util._

class VirtualChannelState extends Bundle {
  import NetworkConfig._
  // val work_state = VirtualChannelWorkingState()
  val target = RouteTarget()
  val output_vc = UInt(log2Ceil(virtual_channels).W)
}

class FlitAndValid extends Bundle {
  import NetworkConfig._
  val flit = new Flit
  val valid = Bool()
  val vc = UInt(log2Ceil(virtual_channels).W) // which local VC this flit belongs to
}

/*  Full signals for a single direction port of the router. */
class RouterPort extends Bundle {
  import NetworkConfig._
  val flit_in = Flipped(Decoupled(new Flit))
  val credit_in = Input(Vec(virtual_channels, UInt(log2Ceil(buffer_depth + 1).W)))
  val flit_out = Decoupled(new Flit)
  val credit_out = Output(Vec(virtual_channels, UInt(log2Ceil(buffer_depth + 1).W)))
}

/*             N
               |
            W--|--E
               |
               S
 */
class Router(x: Int, y: Int) extends Module {
  import NetworkConfig._
  val io = IO(new Bundle {
    val north_port = new RouterPort
    val south_port = new RouterPort
    val west_port = new RouterPort
    val east_port = new RouterPort
    val local_port = new RouterPort
  })

  val allPortsVec = Wire(Vec(5, new RouterPort))
  val inputBuffers = Seq.fill(5)(Module(new InputBuffers))
  val firstStageLogics = Seq.fill(5)(Module(new FirstStageLogic(x, y)))
  val pipelineReg = Seq.fill(5)(RegInit(0.U.asTypeOf(new FlitAndValid)))
  val secondStageLogics = Seq.fill(5)(Module(new SecondStageLogic))
  val vcQueues = Seq.fill(5)(Module(new VirtualChannelQ))
  val vcQueuesWires = Wire(Vec(5, chiselTypeOf(vcQueues(0).io)))
  val vcStates = Seq.fill(5)(RegInit(VecInit.fill(virtual_channels)
                                     (0.U.asTypeOf(new VirtualChannelState))))
  val tailJustLeave = Seq.fill(5)(RegInit(VecInit.fill(virtual_channels)(false.B)))
  val secondStageFire = WireInit(VecInit.fill(5)(false.B))

  // A BIT OF DEFAULT VALUE
  allPortsVec.zip(Seq(io.north_port, io.south_port, io.west_port, io.east_port, io.local_port)).foreach{case(w, p) =>
    w <> p
  }
  vcQueuesWires.zip(vcQueues).foreach{case(w, m) => 
    w <> m.io
    w.enq.bits := 0.U
    w.enq.valid := false.B
    w.deq.ready := false.B
  }
  // connect input ports with input buffers
  allPortsVec.zip(inputBuffers).foreach{case(p, b) => 
    p.flit_in <> b.io.in_flit
    p.credit_out := b.io.room
  }
  // connect input buffers with first stage logic
  inputBuffers.zip(firstStageLogics).foreach{case(b, fl) =>
    fl.io.in_flits <> b.io.out_flits
  }
  // connect first stage logic with pipeline registers
  firstStageLogics.zip(pipelineReg).zip(vcStates).zip(secondStageFire).foreach{case(((fl, r), s), ssf) => 
    fl.io.winner_flit.ready := !r.valid || ssf
    when(fl.io.winner_flit.fire) {
      r.flit := fl.io.winner_flit.bits
      r.flit.header.vc_id := s(fl.io.winner_vc).output_vc // update the vc field
      r.valid := true.B
      r.vc := fl.io.winner_vc
    }.otherwise {
      when(ssf) {
        r.valid := false.B
      }
    }
  }
  // update VC states when head/single flit arrives
  firstStageLogics.zip(vcStates).zip(pipelineReg).foreach{case((fl, s), r) => 
    fl.io.stall.zip(s).foreach{case(stl, state) =>
      stl := allPortsVec(state.target.asUInt).credit_in(state.output_vc) < 2.U 
    }
    fl.io.free_vc := vcQueuesWires.map(q => q.deq.valid)
    when(fl.io.winner_flit.fire) {
      when(fl.io.winner_flit.bits.header.flit_type === FlitTypes.head ||
           fl.io.winner_flit.bits.header.flit_type === FlitTypes.single) {
        s(fl.io.winner_vc).output_vc := vcQueuesWires(fl.io.winner_target.asUInt).deq.bits
        vcQueuesWires(fl.io.winner_target.asUInt).deq.ready := true.B
        s(fl.io.winner_vc).target := fl.io.winner_target
        r.flit.header.vc_id := vcQueuesWires(fl.io.winner_target.asUInt).deq.bits // special treatment
      }
    }
  }
  // connect second stage logics with pipeline registers
  secondStageLogics.zip(0 until 5).foreach{case(sl, t) => 
    sl.io.in_flits.zip(pipelineReg).zip(vcStates).zip(secondStageFire).foreach{case(((i, r), s), ssf) =>
      i.valid := (s(r.vc).target.asUInt === t.U) && r.valid
      i.bits := r.flit
      when(i.fire) {
        // FIXME: this will cause an idle cycle in pipeline
        // r.valid := false.B
        ssf := true.B
      }
    }
  }
  // connect output ports with second stage logics
  allPortsVec.zip(secondStageLogics).foreach{case(p, sl) =>
    p.flit_out <> sl.io.winner_flit
  }
  // maintain free virtual channel queues
  allPortsVec.zip(tailJustLeave).zip(vcQueuesWires).foreach{case((p, leave), q) =>
    when(p.flit_out.fire) {
      when(p.flit_out.bits.header.flit_type === FlitTypes.tail ||
           p.flit_out.bits.header.flit_type === FlitTypes.single) {
        leave(p.flit_out.bits.header.vc_id) := true.B
      }
    }
    p.credit_in.zip(leave).zip(0 until virtual_channels).foreach{case((credit, l), v) =>
      when(credit === buffer_depth.U && l) {
        q.enq.valid := true.B
        q.enq.bits := v.U
        l := false.B
      }
    }
  }
}