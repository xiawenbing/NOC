package mesh_network

import chisel3._
import chisel3.util._

object NetworkConfig {
  val rows = 2
  val columns = 2
  val load_width = 96 // in bits
  val addr_width = 12 
  val data_length_width = 4
  val data_width = addr_width + data_length_width // data = (addr + length)
  val virtual_channels = 2
  val flit_load_width = 128
  val buffer_depth = 8
}

// the address of a router
class Coordinate extends Bundle {
  val x = UInt(log2Ceil(NetworkConfig.columns).W)
  val y = UInt(log2Ceil(NetworkConfig.rows).W)
}
