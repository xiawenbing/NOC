package mesh_network

import chisel3._
import chisel3.util._

// peeking more data in simulation, one for each router
class PeekingSignals extends Bundle {
  import NetworkConfig._
  val free_buffers = UInt(log2Ceil(buffer_depth * virtual_channels * 5).W)
}

/*  
    (0, 1)    (1, 1)
    (0, 0)    (1, 0)

           N
           |
        W--|--E
           |
           S
 */
// collect_data: enable it to collect more data in simulation
class NetworkExample(collect_data: Boolean) extends Module {
  import NetworkConfig._
  require(rows >= 2 && columns >= 2)
  val io = IO(new Bundle{
    val locals = Vec(nodes, new RouterPort)
    val peeking_signals = Vec(if(collect_data) nodes else 0,
                           new PeekingSignals)
  })

  val routers = Util.genAddress.map{case (i, j) => Module(new Router(i, j))}

  def routerConnectNull(r: Router) = {
    r.io.north_port <> 0.U.asTypeOf(new RouterPort)
    r.io.south_port <> 0.U.asTypeOf(new RouterPort)
    r.io.west_port <> 0.U.asTypeOf(new RouterPort)
    r.io.east_port <> 0.U.asTypeOf(new RouterPort)
  }
  def routerConnectRow(left: Router, right: Router) = {
    left.io.east_port.flit_in <> right.io.west_port.flit_out
    left.io.east_port.credit_in := right.io.west_port.credit_out
    right.io.west_port.flit_in <> left.io.east_port.flit_out
    right.io.west_port.credit_in := left.io.east_port.credit_out
  }
  def routerConnectCol(up: Router, down: Router) = {
    up.io.south_port.flit_in <> down.io.north_port.flit_out
    up.io.south_port.credit_in := down.io.north_port.credit_out
    down.io.north_port.flit_in <> up.io.south_port.flit_out
    down.io.north_port.credit_in := up.io.south_port.credit_out
  }
  def connectPeekingSigs(r: Router, s: PeekingSignals) = {
    val all_ports = Seq(r.io.north_port, r.io.south_port, r.io.west_port, r.io.east_port, r.io.local_port)
    s.free_buffers := all_ports.foldLeft(0.U)((num, rp) => num + rp.credit_out.reduce(_ + _))
  }

  routers.foreach(r => routerConnectNull(r))

  (0 until columns - 1).foreach(x => 
    (0 until rows).foreach(y => 
      routerConnectRow(routers(coordinate2Idx(x, y)),
                       routers(coordinate2Idx(x + 1, y)))
    )
  )

  (0 until rows - 1).foreach(y =>
    (0 until columns).foreach(x =>
      routerConnectCol(routers(coordinate2Idx(x, y + 1)),
                       routers(coordinate2Idx(x, y)))
    )
  )

  io.locals.zip(routers).foreach{case(p, r) =>
    p <> r.io.local_port
  }

  if(collect_data) {
    routers.zip(io.peeking_signals).foreach{case(r, sig) => 
      connectPeekingSigs(r, sig)
    }
  }
}

class NetworkExampleWithNI(collect_data: Boolean) extends Module {
  import NetworkConfig._
  val io = IO(new Bundle{
    val local00 = new RouterPort
    val local01 = new SchedulerPort
    val local10 = new SchedulerPort
    val local11 = new SchedulerPort
  })
  val network = Module(new NetworkExample(collect_data))
  val NI01 = Module(new NetworkInterface(0, 1))
  val NI10 = Module(new NetworkInterface(1, 0))
  val NI11 = Module(new NetworkInterface(1, 1))

  def connectNI(r:RouterPort, ni: NetworkInterface, s: SchedulerPort) = {
    r.credit_in := ni.io.router_port.credit_out
    r.flit_in <> ni.io.router_port.flit_out
    ni.io.router_port.credit_in := ni.io.router_port.credit_out
    ni.io.router_port.flit_in <> r.flit_out
    
    ni.io.scheduler_port <> s
  }

  // io.local00 <> network.io.local00
  // connectNI(network.io.local01, NI01, io.local01)
  // connectNI(network.io.local10, NI10, io.local10)
  // connectNI(network.io.local11, NI11, io.local11)

}