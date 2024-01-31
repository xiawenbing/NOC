package mesh_network

import chisel3._
import chisel3.util._

object PacketTypes extends ChiselEnum {
  val packet_route = Value
  val packet_cfg = Value
  val packet_key = Value
  val packet_data = Value
  val packet_command = Value
}

abstract class HeadFlitData extends Bundle

class RouteConfig extends HeadFlitData {
  import SystemConfig._
  // for now we only have unicast
  val next_dest = new Coordinate
}

class DataPacket extends HeadFlitData {
  // val packet_length = ???
  val data = UInt((new HeadFlitLoad).getWidth.W)
}

// software version difinition of a flit for simulation usage
class soft_Flit(val flit_type: FlitTypes.Type, 
                val source: (Int, Int),   //*
                val dest: (Int, Int),     // *
                val packet_type: Int,     // *
                val task_id: Int,         // *
                val load: BigInt) {
}

abstract class soft_Packet {
  def toFlits: List[soft_Flit]
}

// the load of TestPacket's first flit contains packet_id, which is used for 
// collecting statics
class TestPacket(packet_id: Int, 
                 source: (Int, Int),
                 dest: (Int, Int),
                 length: Int, // in flits
                 task_id:Int,
                 load: Array[BigInt] = Array(0)) extends soft_Packet {
  require(length > 0 && load.length <= length)
  override def toFlits: List[soft_Flit] = {
    var flits: List[soft_Flit] = Nil
    val flit_loads = Array.copyOf(load, length)
    for(i <- load.length until length) {
      flit_loads(i) = 0
    }

    if(length == 1) {
      flits = flits.appended(new soft_Flit(FlitTypes.single, source, dest, 3, task_id, packet_id))
    } else {
      flits = flits.appended(new soft_Flit(FlitTypes.head, source, dest, 3, task_id, packet_id))
    }

    for(i <- 1 until length - 1) {
      flits = flits.appended(new soft_Flit(FlitTypes.body, source, dest, 3, task_id, flit_loads(i)))
    }

    if(length > 1) {
      flits = flits.appended(new soft_Flit(FlitTypes.tail, source, dest, 3, task_id, flit_loads(length-1)))
    }
    flits
  }
}

class soft_RouteConfigPacket(source: (Int, Int),
                        dest: (Int, Int),
                        task_id: Int,
                        next_dest: (Int, Int)) extends soft_Packet {
  override def toFlits: List[soft_Flit] = {
    val next_dest_x = Util.wrapWithPadding0(next_dest._1, log2Ceil(NetworkConfig.columns))
    val next_dest_y = Util.wrapWithPadding0(next_dest._2, log2Ceil(NetworkConfig.rows))
    val data = BigInt(next_dest_x + next_dest_y, 2)
    val flit = new soft_Flit(FlitTypes.single, source, dest, 0, task_id, data)
    List(flit)
  }
}

class soft_MemCtrlrPacket(source: (Int, Int),
                          dest: (Int, Int),
                          task_id: Int,
                          r_or_w: Boolean,
                          length: Int) extends soft_Packet {
  override def toFlits: List[soft_Flit] = {
    val rw = if(r_or_w) 1.toString else 0.toString
    val length_str = BigInt(length).toString(2)
    val data = BigInt(rw + length_str, 2)
    val flit = new soft_Flit(FlitTypes.single, source, dest, 4, task_id, data)
    List(flit)
  }
}


// this is used for NI testing, the NI can handle the source and dest field
class soft_DataPacket(task_id: Int,
                      data: BigInt = 0) extends soft_Packet {
  override def toFlits: List[soft_Flit] = {
    val flit = new soft_Flit(FlitTypes.single, (0, 0), (0, 0), 3, task_id, data)
    List(flit)
  }
}