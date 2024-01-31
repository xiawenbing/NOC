package mesh_network

import chisel3._
import chisel3.util._
import chiseltest._
import scala.collection.mutable.ArraySeq

class StatInfo

abstract class EndNodeDevice {
  val receive_buffer: ArraySeq[List[soft_Flit]] = ArraySeq.fill(NetworkConfig.virtual_channels)(Nil)
  // handles the port behavior at each device, including valid/ready signals
  def tick(port: RouterPort): StatInfo

  // convert flits to a packet
  def flitsToPacket(flits: List[soft_Flit]): soft_Packet = {
    throw new RuntimeException("WRONG TARGET")
  }
}

class IdleDevice extends EndNodeDevice {
  override def tick(port: RouterPort): StatInfo = {
    // does nothing
    new StatInfo
  }
}

class MasterDevice extends EndNodeDevice {
  override def tick(port: RouterPort): StatInfo = {
    // does nothing
    new StatInfo
  }
}

class MemoryController extends EndNodeDevice {
  var r_data_length = 0
  var r_dest_node = 0
  var w_data_length = 0
  override def tick(port: RouterPort): StatInfo = {
    // check input
    if(port.flit_in.valid.peekBoolean()) {
      // TODO!
    }

    new StatInfo
  }

  override def flitsToPacket(flits: List[soft_Flit]): soft_Packet = {
    require(flits.head.flit_type == FlitTypes.head ||
            flits.head.flit_type == FlitTypes.single)
    import Util._
    import helper._
    import NetworkConfig._
    import SystemConfig._

    // val data_width: Int = flit_load_width - 2 * (log2Ceil(columns) + log2Ceil(rows)) -
    //                       PacketTypes.getWidth - log2Ceil(max_tasks)

    flits.head.packet_type match {
      case 0 /* route */ => {
        require(flits.length == 1)
        val flit = flits.head
        val data_str = wrapWithPadding0(flit.load, log2Ceil(columns) + log2Ceil(rows))
        val next_dest_x = conv(data_str.substring(0, log2Ceil(columns)))
        val next_dest_y = conv(data_str.substring(log2Ceil(columns), log2Ceil(columns) + log2Ceil(rows)))
        new soft_RouteConfigPacket(flit.source, flit.dest, flit.task_id, (next_dest_x, next_dest_y))
      }
      case 3 /* data */ => {
        val flit = flits.head
        new TestPacket(flit.load.toInt, flit.source, flit.dest, 1,flits.length)
      }
      case 4 /* command */ => {
        require(flits.length == 1)
        val flit = flits.head
        val data_str = wrapWithPadding0(flit.load, log2Ceil(columns) + log2Ceil(rows))
        val r_or_w = conv(data_str.substring(0, 1)) == 1
        val length = conv(data_str.substring(1, data_str.length()))
        new soft_MemCtrlrPacket(flit.source, flit.dest, flit.task_id, r_or_w, length)
      }
      case _ => throw new RuntimeException("!unsupported packet type!")
    }
  }
}