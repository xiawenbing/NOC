package mesh_network

import chisel3._
import chisel3.util._
import scala.math.BigDecimal._
import scala.math._

object NetworkConfig {
  val rows = 3
  val columns = 3
  val nodes = rows * columns
  val virtual_channels = 4
  val flit_load_width = 128
  val buffer_depth = 4

  val task_id_with = 4
  val currpackagelength_width = 16
  val tagpackagelenth_width = 16
  val priority_width = 4
  val finishpacklength_width = 16
  def idx2Coordinate(idx: Int): (Int, Int) = {
    val x = idx / rows
    val y = idx % rows
    (x, y)
  }
  def coordinate2Idx(c: (Int, Int)): Int = c match {
    case (x, y) => x * rows + y
  }
}

object SystemConfig {
  // each task belongs to a user; a user could own multiple tasks
  // user id should be used for key indexing
  // task id should be used in most other situation
  val max_users = 8
  val max_tasks = 4
  
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

  // convert a "small" binary string to Int
  def conv(s: String): Int = BigInt(s, 2).toInt

  // generate a sequence of router address
  def genAddress: Seq[(Int, Int)] = (0 until NetworkConfig.nodes).
    map{NetworkConfig.idx2Coordinate(_)}

    // scale: the decimal places to keep
  def fixedPrecisionDouble(in: Double, scale: Int): Double = 
    BigDecimal(in).setScale(scale, RoundingMode.HALF_UP).toDouble

  // the distance between two routers (in hops)
  def getDistance(src: (Int, Int), dest: (Int, Int)): Int = {
    def abs(in: Int): Int = if(in > 0) in else -in
    val abs_x = abs(src._1 - dest._1)
    val abs_y = abs(src._2 - dest._2)
    abs_x + abs_y
  }
}