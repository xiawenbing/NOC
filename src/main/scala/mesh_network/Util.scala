package mesh_network

import chisel3._
import chisel3.util._

object NetworkConfig {
  val rows = 2
  val columns = 2
  val nodes = rows * columns
  val virtual_channels = 2
  val flit_load_width = 128
  val buffer_depth = 8

  def idx2Coordinate(idx: Int): (Int, Int) = {
    val x = idx / rows
    val y = idx % rows
    (x, y)
  }
}

object SystemConfig {
  // each task belongs to a user; a user could own multiple tasks
  // user id should be used for key indexing
  // task id should be used in most other situation
  val max_users = 32
  val max_tasks = max_users
  
}

// the address of a router
class Coordinate extends Bundle {
  val x = UInt(log2Ceil(NetworkConfig.columns).W)
  val y = UInt(log2Ceil(NetworkConfig.rows).W)
}

object Util {
  def getPaddingString(num: BigInt, length: Int): String = {
    val numString = num.toString(2)
    val paddingLength = length - numString.length
    if (paddingLength <= 0) {
      "" // No need to extend
    } else {
      "0" * paddingLength
    }
  }

  // convert a BigInt into binary string, with padding zeros to a given length
  def wrapWithPadding0(num: BigInt, length: Int): String = {
    getPaddingString(num, length) + num.toString(2)
  }
}