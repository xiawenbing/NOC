package mesh_network

import chisel3._
import chisel3.util._
import helper._
import scala.util.Random

/*  CLASSIC TRAFIC PATTERNS

    RANDOM: (also known as uniform) In this kind of pattern, source and destination is selected randomly for each message generated.

    BIT REVERSAL: Fixed source-destination pair for every message. The node with binary value an-1, an-2,..., a1, a0 communicates 
                  with the node a0, a1,..., an-2, an-1.

    PERFECT SHUFFLE: Fixed source-destination pair for every message. The node with binary value an-1, an-2, ..., a1, a0 
                     communicates with the node an-2, an-3, ..., a0, an-1 (rotate left 1 bit).

    TRANSPOSE MATRIX: Fixed source-destination pair for every message. 
                      The node with binary value an-1, an-2, ..., a1, a0 communicates with the node an/2-1,..., a0, a,n-1, ..., an/2.

    TORNADO: Fixed source-destination pair for every message. The node with decimal coordinates [x,y] (bi-dimensional), communicates 
             with the node [(x+(k/2-1)) mod k, (y+(k/2-1)) mod k], where k represents the size of the network in both x and y dimensions.

    LOCAL: Source node is generated randomly for every message. Destination is also generated randomly, but an additional condition must
           be accomplished. Destination has to be at a maximum distance of sigma from source node, 
           being sigma a value that must be defined in the traffic tag.
 */

abstract class TrafficPattern {
  def genPacket(pkt_id: Int, source: (Int, Int)): Option[soft_Packet]
}

class UniformTraffic(injection_rate: Double,
                     max_length: Int) extends TrafficPattern {
  import NetworkConfig._
  require((injection_rate > 0 || injection_rate <= 1) && 
          max_length >= 1)
  def genPacket(pkt_id: Int, source: (Int, Int)): Option[soft_Packet] = {
    if(Random.nextInt(100) < injection_rate * 100) {
      var dest: (Int, Int) = (Random.nextInt(columns), Random.nextInt(rows))
      while(dest == source) {
        dest = (Random.nextInt(columns), Random.nextInt(rows))
      }
      Some(new TestPacket(pkt_id, source, dest, Random.nextInt(max_length) + 1))
    } else {
      None
    }
  }
}

class TornatoTraffic(injection_rate: Double,
                     max_length: Int) extends TrafficPattern {
  import NetworkConfig._
  require((injection_rate > 0 || injection_rate <= 1) && 
          max_length >= 1 &&
          rows > 2 && columns > 2)
  def genPacket(pkt_id: Int, source: (Int, Int)): Option[soft_Packet] = {
    if(Random.nextInt(100) < injection_rate * 100) {
      val dest_x: Int = (source._1 + (columns / 2 - 1)) % columns
      val dest_y: Int = (source._2 + (rows / 2 - 1)) % rows
      var dest: (Int, Int) = (dest_x, dest_y)
      Some(new TestPacket(pkt_id, source, dest, Random.nextInt(max_length) + 1))
    } else {
      None
    }
  }            
}

// TODO: support multiple hot spots
class HotSpotTraffic(injection_rate: Double,
                     max_length: Int,
                     hot_spot_node: (Int, Int)) extends TrafficPattern {
  import NetworkConfig._
  require((injection_rate > 0 || injection_rate <= 1) && 
          max_length >= 1 &&
          hot_spot_node._1 < columns && hot_spot_node._2 < rows)
  def genPacket(pkt_id: Int, source: (Int, Int)): Option[soft_Packet] = {
    if(source == hot_spot_node && Random.nextInt(100) < injection_rate * 100) {
      var dest: (Int, Int) = (Random.nextInt(columns), Random.nextInt(rows))
      while(dest == source) {
        dest = (Random.nextInt(columns), Random.nextInt(rows))
      }
      Some(new TestPacket(pkt_id, source, dest, Random.nextInt(max_length) + 1))
    } else {
      None
    }
  }
}