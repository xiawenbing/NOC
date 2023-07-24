package tree

import chisel3._
import chisel3.util._

import exampleIPs.SBoxWrapper

object ShiftRegisterInit {
  def apply[T <: Data](next: T, n: Int, init: T): T = {
    if (n == 1) {
      RegNext(next, init)
    } else if (n > 0) {
      RegNext(apply(next, n - 1, init), init)
    } else {
      next
    }
  }
}

class emptyBox extends Module {
  val io = IO(new Bundle{
    val in = Flipped(ValidIO(UInt(32.W)))
    val out = ValidIO(UInt(32.W))
  })
  when(io.in.valid) {
    io.out.bits := ShiftRegisterInit(Cat(io.in.bits, io.in.bits, 
                                          io.in.bits, io.in.bits), 4, 0.U)(31, 0)
  }.otherwise {
    io.out.bits := 0.U
  }
  io.out.valid := io.out.bits(0)
}

class tree4 extends Module {
  val (width, depth, forks) = (32, 1, 4)
  val io = IO(new Bundle {
    val input = Flipped(ValidIO(new ForkBundle(width, depth, forks)))
    val output = ValidIO(new ForkBundle(width, depth, forks))
  })
  val r = Module(new Router(width, depth, forks))
  val b = Seq.fill(4)(Module(new SBoxWrapper))
  for(i <- 0 until 4) {
    b(i).io.input.bits := r.iio.output(i).bits.data
    b(i).io.input.valid := r.iio.output(i).valid
    r.oio.input(i).bits.data := b(i).io.output.bits
    r.oio.input(i).valid := b(i).io.output.valid
    r.oio.input(i).bits.addr := DontCare
  }
  io.input <> r.iio.input
  io.output <> r.oio.output
}

class tree16 extends Module {
  val (width, depth, forks) = (32, 2, 4)
  val io = IO(new Bundle {
    val input = Flipped(ValidIO(new ForkBundle(width, depth, forks)))
    val output = ValidIO(new ForkBundle(width, depth, forks))
  })
  val r = Module(new Router(width, depth, forks))
  val st = Seq.fill(4)(Module(new tree4))
  for(i <- 0 until 4) {
    st(i).io.input <> r.iio.output(i)
    r.oio.input(i) <> st(i).io.output
  }
  io.input <> r.iio.input
  io.output <> r.oio.output
}

class tree32 extends Module {
  val (width, depth, forks) = (32, 3, 4)
  val io = IO(new Bundle {
    val input = Flipped(ValidIO(new ForkBundle(width, depth, forks)))
    val output = ValidIO(new ForkBundle(width, depth, forks))
  })
  val r = Module(new Router(width, depth, forks))
  val st = Seq.fill(2)(Module(new tree16))
  for(i <- 0 until 2) {
    st(i).io.input <> r.iio.output(i)
    r.oio.input(i) <> st(i).io.output
  }
  for(i <- 2 until 4) {
    r.oio.input(i) := DontCare
  }
  io.input <> r.iio.input
  io.output <> r.oio.output
}

class tree64 extends Module {
  val (width, depth, forks) = (32, 3, 4)
  val io = IO(new Bundle {
    val input = Flipped(ValidIO(new ForkBundle(width, depth, forks)))
    val output = ValidIO(new ForkBundle(width, depth, forks))
  })
  val r = Module(new Router(width, depth, forks))
  val st = Seq.fill(4)(Module(new tree16))
  for(i <- 0 until 4) {
    st(i).io.input <> r.iio.output(i)
    r.oio.input(i) <> st(i).io.output
  }
  io.input <> r.iio.input
  io.output <> r.oio.output
}

class tree128 extends Module {
  val (width, depth, forks) = (32, 4, 4)
  val io = IO(new Bundle {
    val input = Flipped(ValidIO(new ForkBundle(width, depth, forks)))
    val output = ValidIO(new ForkBundle(width, depth, forks))
  })
  val r = Module(new Router(width, depth, forks))
  val st = Seq.fill(2)(Module(new tree64))
  for(i <- 0 until 2) {
    st(i).io.input <> r.iio.output(i)
    r.oio.input(i) <> st(i).io.output
  }
  for(i <- 2 until 4) {
    r.oio.input(i) := DontCare
  }
  io.input <> r.iio.input
  io.output <> r.oio.output
}
