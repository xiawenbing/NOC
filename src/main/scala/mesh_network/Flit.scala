package mesh_network

import chisel3._
import chisel3.util._

object FlitTypes extends ChiselEnum {
  val head = Value
  val body = Value
  val tail = Value
  val single = Value
}

class FlitHeader extends Bundle {
  val flit_type = FlitTypes()  //2bit
  val vc_id = UInt(log2Ceil(NetworkConfig.virtual_channels).W) //2bit
  val ID = UInt(log2Ceil(SystemConfig.max_users).W)  //3bit  ->2bit
  val priority = UInt(log2Ceil(NetworkConfig.priority_width).W) //2bit
  val currpackagelength = UInt(log2Ceil(NetworkConfig.currpackagelength_width).W) //4bit
  val tagpackalength = UInt(log2Ceil(NetworkConfig.tagpackagelenth_width).W) //4bit
}

abstract class FlitLoad extends Bundle 


class HeadFlitLoad extends FlitLoad {
  import NetworkConfig._
  val source = new Coordinate
  val dest = new Coordinate
  val packet_type = UInt((PacketTypes.getWidth).W) // Use UInt instead of PacketTypes since sbt complains when casting from UInt to PacketTypes
  val task_id = UInt(log2Ceil(SystemConfig.max_tasks).W)
  def data_width: Int = flit_load_width - source.getWidth - 
                        dest.getWidth - packet_type.getWidth - task_id.getWidth
  val data = Bits(data_width.W) // this field will be decoded based on packet_type
}


class DataFlitLoad extends FlitLoad {
  val data = Bits(NetworkConfig.flit_load_width.W)
}

class Flit extends Bundle {
  val header = new FlitHeader
  val load = Bits(NetworkConfig.flit_load_width.W)
}

class Flit_head extends Bundle {
  val header = new FlitHeader
}

class operterBundle extends  Bundle {
  import NetworkConfig._
    val tagpackalength = UInt(log2Ceil(SystemConfig.max_tasks).W)
    val currlength = UInt(log2Ceil(NetworkConfig.priority_width).W)
    val ID = UInt(log2Ceil(NetworkConfig.tagpackagelenth_width).W)
    def data_width: Int = flit_load_width - tagpackalength.getWidth - 
                        currlength.getWidth - ID.getWidth
    val data = UInt(data_width.W)
}




