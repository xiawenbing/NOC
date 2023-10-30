package mesh_network

import chisel3._
import chisel3.util._
import chiseltest._
import java.math.BigInteger

object helper {

  def getPaddingString(num: BigInt, length: Int): String = {
    val numString = num.toString(2)
    val paddingLength = length - numString.length
    if (paddingLength <= 0) {
      "" // No need to extend
    } else {
      "0" * paddingLength
    }
  }

  // poke router flit port, without ready-valid handshake
  def pokeFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int)(implicit clk: Clock): Int = {
    var cycle_spent = 0
    // port.valid.poke(true)
    port.bits.header.flit_type.poke(flit.flit_type)
    port.bits.header.vc_id.poke(vc_id)
    // TODO: handle flit load
    import NetworkConfig._
    flit.flit_type match {
      case FlitTypes.head | FlitTypes.single => {
        val columns = log2Ceil(NetworkConfig.columns)
        val rows = log2Ceil(NetworkConfig.rows)
        val source_x = Integer.toBinaryString(flit.source._1)
        val source_y = Integer.toBinaryString(flit.source._2)
        val dest_x = Integer.toBinaryString(flit.dest._1)
        val dest_y = Integer.toBinaryString(flit.dest._2)
        val data = flit.load.toString(2)
        val padding = getPaddingString(flit.load,
                                       flit_load_width - 2 * columns - 2 * rows)
        port.bits.load.poke(BigInt(source_x + source_y + 
                                   dest_x + dest_y + padding + data, 2))
      }
      case FlitTypes.body | FlitTypes.tail => {
        port.bits.load.poke(flit.load)
      }
    }
    cycle_spent
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