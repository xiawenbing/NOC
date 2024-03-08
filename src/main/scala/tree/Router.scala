package tree

// import math.{log, ceil}
import chisel3._
import chisel3.util._

class ForkBundle(width: Int, depth: Int, forks: Int) extends Bundle {
  val data = UInt(width.W)
  val addr = UInt(/* (depth * ceil(log(forks) / log(2)).toInt) */log2Ceil(forks).W) // if it's 0, then this port won't exist in the resulting Verilog
}

class InputRouter(width: Int, depth: Int, forks: Int) extends Module {
  assert(depth > 0)
  val addr_width = /* ceil(log(forks) / log(2)).toInt */ log2Ceil(forks)
  val io = IO(new Bundle {
    val input = Flipped(ValidIO(new ForkBundle(width, depth, forks)))
    val output = Vec(forks, ValidIO(new ForkBundle(width, depth - 1, forks)))
  })
  def GetCurrentAddr(in: UInt): UInt = in(in.getWidth - 1, in.getWidth - addr_width)
  def DropCurrentAddr(in: UInt): UInt = in(in.getWidth - addr_width - 1, 0)
  for(i <- 0 until forks){
    io.output(i).valid := false.B
    io.output(i).bits := DontCare // if the addr field does not exist, the compiler will simply ignore it
  }
  when(io.input.valid) {
    val select = GetCurrentAddr(io.input.bits.addr)
    val selected = io.output(select)
    selected.valid := true.B
    selected.bits.data := io.input.bits.data
    if(selected.bits.addr.getWidth != 0) { // still have address to pass
      selected.bits.addr := DropCurrentAddr(io.input.bits.addr)
    }
  }
}

// class OutputRouter(width: Int, depth: Int, forks: Int) extends Module {
//   assert(depth > 0)
//   val io = IO(new Bundle {
//     val input = Vec(forks, Flipped(ValidIO(new ForkBundle(width, depth - 1, forks))))
//     val output = ValidIO(new ForkBundle(width, depth, forks))
//   })
//   val select = MuxCase(0.U, io.input.zip(0 until forks).map{case (en, idx) => (en.valid, idx.U)})
//   val selected = io.input(select)
//   io.output.valid := selected.valid
//   io.output.bits.data := selected.bits.data
//   io.output.bits.addr := Cat(select, selected.bits.addr)
// }

class OutputRouter(width: Int, depth: Int, forks: Int) extends Module {
  assert(depth > 0)
  val io = IO(new Bundle {
    val input = Vec(forks, Flipped(ValidIO(new ForkBundle(width, depth - 1, forks))))
    val output = ValidIO(new ForkBundle(width, depth, forks))
  })
  io.output.valid := false.B
  io.output.bits := DontCare
  for(i <- 0 until forks) {
    when(io.input(i).valid) {
      io.output.valid := true.B
      io.output.bits.data := io.input(i).bits.data
      io.output.bits.addr := Cat(i.U, io.input(i).bits.addr) // current addr always as MSB
    }
  }
}

class Router(width: Int, depth: Int, forks: Int) extends Module {
  assert(depth > 0)
  val iio = IO(new Bundle {
    val input = Flipped(ValidIO(new ForkBundle(width, depth, forks)))
    val output = Vec(forks, ValidIO(new ForkBundle(width, depth - 1, forks)))
  })
  val oio = IO(new Bundle {
    val input = Vec(forks, Flipped(ValidIO(new ForkBundle(width, depth - 1, forks))))
    val output = ValidIO(new ForkBundle(width, depth, forks))
  })
  val i = Module(new InputRouter(width, depth, forks))
  val o = Module(new OutputRouter(width, depth, forks))
  iio <> i.io
  oio <> o.io
}

class Adder(width: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val is_sub = Input(Bool())
    val result = Output(UInt(width.W))
  })
  val b_inv = Mux(io.is_sub, ~io.b, io.b)
  io.result := io.a + b_inv + io.is_sub
}

class Counter(width: Int) extends Module {
  val io = IO(new Bundle {
    val run = Input(Bool())
    val output = Output(UInt(width.W))
  })
  val counter_reg = RegInit(0.U(width.W))
  when(io.run) {
    counter_reg := counter_reg + 1.U
  }
  io.output := counter_reg
}

class Outter1(width: Int, forks: Int) extends Module {
  val io = IO(new Bundle {
    val input = Vec(forks, Flipped(ValidIO(UInt(width.W))))
    val output = ValidIO(UInt(width.W))
  })
  // io.output.valid := false.B
  // io.output.bits := DontCare
  // for(i <- 0 until forks) {
  //   when(io.input(i).valid) {
  //     io.output.valid := true.B
  //     io.output.bits := io.input(i).bits
  //   }
  // }
  io.output.bits := Mux1H(io.input.map(_.valid), io.input.map(_.bits))
  io.output.valid := io.input.map(_.valid).reduce(_|_)
}

class Outter2(width: Int, forks: Int) extends Module {
  val io = IO(new Bundle {
    val input = Vec(forks, Flipped(ValidIO(UInt(width.W))))
    val output = ValidIO(UInt(width.W))
  })
  io.output.valid := false.B
  io.output.bits := DontCare
  for(i <- 0 until forks) {
    when(io.input(i).valid) {
      io.output.valid := true.B
      io.output.bits := io.input(i).bits
    }
  }
}