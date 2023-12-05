package mesh_network

import chisel3._
import chisel3.util._
import chiseltest._
import scala.util.Random
import java.math.BigInteger
import chisel3.internal.throwException

object helper {

  // poke router flit port, without ready-valid handshake
  def pokeFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
    // port.valid.poke(true)
    port.bits.header.flit_type.poke(flit.flit_type)
    port.bits.header.vc_id.poke(vc_id)
    // the diverse of headflit data is handled in toFlits methods instead of here.
    import NetworkConfig._
    import SystemConfig._
    import Util._
    flit.flit_type match {
      case FlitTypes.head | FlitTypes.single => {
        val columns_width = log2Ceil(columns)
        val rows_width = log2Ceil(rows)
        val source_x = wrapWithPadding0(flit.source._1, columns_width)
        val source_y = wrapWithPadding0(flit.source._2, rows_width)
        val dest_x = wrapWithPadding0(flit.dest._1, columns_width)
        val dest_y = wrapWithPadding0(flit.dest._2, rows_width)
        val packet_type = wrapWithPadding0(flit.packet_type, PacketTypes.getWidth)
        val task_id = wrapWithPadding0(flit.task_id, log2Ceil(max_tasks))
        val data = wrapWithPadding0(flit.load,
                                    flit_load_width - 2 * columns_width - 2 * rows_width -
                                    PacketTypes.getWidth - log2Ceil(max_tasks))
        port.bits.load.poke(BigInt(source_x + source_y + dest_x + dest_y +
                                   packet_type + task_id + data, 2))
      }
      case FlitTypes.body | FlitTypes.tail => {
        port.bits.load.poke(flit.load)
      }
    }
  }

  // peek a flit port and convert it back to soft_Flit
  def peekFlit(port: DecoupledIO[Flit]): (Int, soft_Flit) = {
    import Util._
    import helper._
    import NetworkConfig._
    import SystemConfig._
    val flit_type = port.bits.header.flit_type.peek()
    val vc_id = port.bits.header.vc_id.peekInt()
    val load_ = port.bits.load.peekInt()
    val (source, dest, packet_type, task_id, load) = if (flit_type == FlitTypes.head || flit_type == FlitTypes.single) {
      val columns_width = log2Ceil(columns)
      val rows_width = log2Ceil(rows)
      val load_str = wrapWithPadding0(load_, flit_load_width)
      val source_x = load_str.substring(0, columns_width)
      val source_y = load_str.substring(columns_width, columns_width + rows_width)
      val dest_x = load_str.substring(columns_width + rows_width, 2 * columns_width + rows_width)
      val dest_y = load_str.substring(2 * columns_width + rows_width, 2 * columns_width + 2 * rows_width)
      val b = 2 * columns_width + 2 * rows_width
      val packet_type = load_str.substring(b, b + PacketTypes.getWidth)
      val task_id = load_str.substring(b + PacketTypes.getWidth, b + PacketTypes.getWidth + log2Ceil(max_tasks))
      val data = load_str.substring(b + PacketTypes.getWidth + log2Ceil(max_tasks), flit_load_width)
      val source_ = (conv(source_x), conv(source_y))
      val dest_ = (conv(dest_x), conv(dest_y))
      (source_, dest_, conv(packet_type), conv(task_id), BigInt(data, 2))
    } else {
      ((0, 0), (0, 0), 0, 0, load_)
    }

    (task_id, new soft_Flit(flit_type, source, dest, packet_type, task_id, load))
  }

  // peek the port and return a list of free VCs (only used when corresponding 
  // injection_buffer is empty)
  def freeVCs(p: RouterPort): List[Int] = {
    var res: List[Int] = Nil
    for(i <- 0 until NetworkConfig.virtual_channels) {
      if(p.credit_out(i).peekInt() == NetworkConfig.buffer_depth) {
        res = i :: res
      }
    }
    res
  }

  // randomly pick a VC from freeVC list 
  def pickVC(vcs: List[Int]): Int = {
    val idx = Random.nextInt(vcs.length)
    vcs(idx)
  }

  def extractPacketID(f: Flit): BigInt = {
    f.load.peekInt()
  }

  def pokeVec[T <: Data](v: Vec[T], seq: Seq[T]) = {
    require(v.length == seq.length)
    v.zip(seq).foreach{case (port, input) => port.poke(input)}
  }

  def initDecoupled(io: Bundle, clk: Clock) = {
    io.elements.foreach { case (name, data) =>
      data match {
        case decoupled: DecoupledIO[_] => {
          decoupled.setSourceClock(clk)
          decoupled.setSinkClock(clk)
        }
        case _ => { }
      }
    }
  }

  def enqueueNoData[T <: Data](d: DecoupledIO[T])(implicit clk: Clock) = {
    d.valid.poke(true)
    while(d.ready.peek().litToBoolean == false) {
      clk.step()
    }
    clk.step()
    d.valid.poke(false)
  }

  def enqueue[T <: Data](d: DecoupledIO[T], data:T)(implicit clk: Clock) = {
    val previous = d.bits.peek()
    d.bits.poke(data)
    enqueueNoData(d)
    d.bits.poke(previous)
  }

  def enqueueSeq[T <: Data](d: DecoupledIO[T], seq: Seq[T])(implicit clk: Clock) = {
    seq.foreach(data => enqueue(d, data))
  }

  def enqueueVec[T <: Data](d: DecoupledIO[Vec[T]], vec: Seq[T])(implicit clk: Clock) = {
    require(d.bits.length == vec.length)
    for(i <- 0 until d.bits.length) {
      d.bits(i).poke(vec(i))
    }
    enqueueNoData(d)
  }

  def enqueueVecSeq[T <: Data](d: DecoupledIO[Vec[T]], seqvec: Seq[Seq[T]])(implicit clk: Clock) = {
    require(seqvec.map(_.length == d.bits.length).reduce(_ & _))
    seqvec.foreach(seq => enqueueVec(d, seq))
  }

}