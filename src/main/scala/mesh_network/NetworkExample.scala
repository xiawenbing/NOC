package mesh_network
import NetworkInterface._
import chisel3._
import chisel3.util._

// peeking more data in simulation, one for each router
class PeekingSignals extends Bundle {
  import NetworkConfig._
  val free_buffers = UInt(log2Ceil(buffer_depth * virtual_channels * 5 + 1).W)
  val north_out_used = Bool()
  val south_out_used = Bool()
  val west_out_used = Bool()
  val east_out_used = Bool()
}

/*  2x2 mesh
    (0, 1)    (1, 1)
    (0, 0)    (1, 0)

     3x3 mesh

    (0, 2)    (1, 2)    (2, 2)
    (0, 1)    (1, 1)    (2, 1)
    (0, 0)    (1, 0)    (2, 0)  

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
    s.free_buffers := all_ports.foldLeft(0.U)((num, rp) => num +& rp.credit_out.reduce(_ +& _))
    s.north_out_used := r.io.north_port.flit_out.valid
    s.south_out_used := r.io.south_port.flit_out.valid
    s.west_out_used := r.io.west_port.flit_out.valid
    s.east_out_used := r.io.east_port.flit_out.valid
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


/*
     3x3 mesh

    (0, 2)    (1, 2)    (2, 2)
    (0, 1)    (1, 1)    (2, 1)
    (0, 0)    (1, 0)    (2, 0)  
*/
class NetworkExampleWithNI(collect_data: Boolean) extends Module {
  import NetworkConfig._
  val io = IO(new Bundle{
    val local00 = new RouterPort
    // val local01 = new SchedulerPort
    //val local10 = new SchedulerPort
    //val local11 = new SchedulerPort
/******************************************/
    //val local02 = new SchedulerPort
    //val local12 = new SchedulerPort
    val local22 = new SchedulerPort
    //val local20 = new SchedulerPort
    val local21 = new SchedulerPort
/******************************************/
    val runned01 = Output(UInt(3.W))
    val runned10 = Output(UInt(3.W))
    val runned11 = Output(UInt(3.W))
    val runned02 = Output(UInt(3.W))  
    val runned12 = Output(UInt(3.W))
    val runned20 = Output(UInt(3.W))
    val sm311tcpcount = Output(UInt(3.W))
  })
  val network = Module(new NetworkExample(collect_data))
  val NI01 = Module(new NetworkInterface(0, 1))
  val NI10 = Module(new NetworkInterface(1, 0))
  val NI11 = Module(new NetworkInterface(1, 1))

  val NI02 = Module(new NetworkInterface(0, 2))
  val NI12 = Module(new NetworkInterface(1, 2))
  val NI22 = Module(new NetworkInterface(2, 2))
  val NI20 = Module(new NetworkInterface(2, 0))
  val NI21 = Module(new NetworkInterface(2, 1))

  val Top_sm4 = Module(new tesSM4CCnetworkadaptor(4))

  val Top_sm3 = Module(new Top_sm3CCnetworkadaptor(4))
  val Top_sm3_512 = Module(new Top_sm3CCnetworkadaptor512(4))
  val Top_pot = Module(new Top_potCCnetworkadaptor(4))
  val Top_addsup = Module(new Top_addsupnetworkadaptor(4))
  val Top_muliv = Module(new Top_mulivnetworkadaptor(4))
//01挂着sm4 1
  for(i<-0 until 4){
    NI01.io.scheduler_port.flits_out(i)<>Top_sm4.io.inputs(i)
  }
  Top_sm4.io.output<>NI01.io.scheduler_port.flit_in

  io.runned01:=Top_sm4.io.runCount  //记录
  connectopertor(network.io.locals(1),NI01)

  //10挂着sm3 3
  for(i<-0 until 4){
    NI10.io.scheduler_port.flits_out(i)<>Top_sm3.io.inputs(i)
  }
  Top_sm3.io.output<>NI10.io.scheduler_port.flit_in
  io.runned10:=Top_sm3.io.runCount
  connectopertor(network.io.locals(3),NI10)



  // 11挂着sm3 512 4
  for(i<-0 until 4){  
    NI11.io.scheduler_port.flits_out(i)<>Top_sm3_512.io.inputs(i)
  }
  Top_sm3_512.io.output<>NI11.io.scheduler_port.flit_in
   io.runned11:=Top_sm3_512.io.runCount
   io.sm311tcpcount:=Top_sm3_512.io.tcpCount
  connectopertor(network.io.locals(4),NI11)


  //02挂着pot 2
  for(i<-0 until 4){
    NI02.io.scheduler_port.flits_out(i)<>Top_pot.io.inputs(i)
  }
  Top_pot.io.output<>NI02.io.scheduler_port.flit_in
  io.runned02:=Top_pot.io.runCount
  connectopertor(network.io.locals(2),NI02)

//12挂着点加 5
  for(i<-0 until 4){
    NI12.io.scheduler_port.flits_out(i)<>Top_addsup.io.inputs(i)
  }
  Top_addsup.io.output<>NI12.io.scheduler_port.flit_in
  io.runned12:=Top_addsup.io.runCount
  connectopertor(network.io.locals(5),NI12)

//20挂着模逆 6
  for(i<-0 until 4){
    NI20.io.scheduler_port.flits_out(i)<>Top_muliv.io.inputs(i)
  }
  Top_muliv.io.output<>NI20.io.scheduler_port.flit_in
  io.runned20:=Top_muliv.io.runCount
  connectopertor(network.io.locals(6),NI20)



  def connectopertor(r:RouterPort, ni: NetworkInterface){
    r.credit_in := ni.io.router_port.credit_out
    r.flit_in <> ni.io.router_port.flit_out
    ni.io.router_port.credit_in := ni.io.router_port.credit_out
    ni.io.router_port.flit_in <> r.flit_out
  }



  def connectNI(r:RouterPort, ni: NetworkInterface, s: SchedulerPort) = {
    r.credit_in := ni.io.router_port.credit_out
    r.flit_in <> ni.io.router_port.flit_out
    ni.io.router_port.credit_in := ni.io.router_port.credit_out
    ni.io.router_port.flit_in <> r.flit_out
    
    ni.io.scheduler_port <> s
  }

  io.local00 <> network.io.locals(0)
   //connectNI(network.io.locals(2), NI02, io.local02)
   //connectNI(network.io.locals(5), NI12, io.local12)
   connectNI(network.io.locals(8), NI22, io.local22)
   connectNI(network.io.locals(7), NI21, io.local21)
   //connectNI(network.io.locals(6), NI20, io.local20)
  //connectNI(network.io.locals(2), NI10, io.local10)
  //connectNI(network.io.locals(3), NI11, io.local11)

}