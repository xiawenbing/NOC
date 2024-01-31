package mesh_network
import scala.util.Random
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import treadle.executable.Big
import org.scalatest._
import NetworkInterface._
import  crypto_ips._
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration.Duration
import scala.concurrent.{Future, Promise, Await}
import scala.util.control.Breaks._
// class NetworkInterfaceSpec extends AnyFreeSpec with ChiselScalatestTester {
//   "NetworkInterface" in {
//     test(new NetworkInterface(1, 1)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
//       def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
//         port.valid.poke(true)
//         helper.pokeFlit(port, flit, vc_id)
//         dut.clock.step()
//         port.valid.poke(false)
//       }
//       import helper._
//       implicit val clk = dut.clock
//       implicit val d = dut

//       dut.io.router_port.credit_in.foreach{p => p.poke(8)}
//       dut.io.router_port.flit_out.ready.poke(true)

//       dut.clock.step(3)
//       val pkt1 = (new soft_RouteConfigPacket((0, 0), (1, 1), 5, (1, 0))).toFlits
//       enqFlit(dut.io.router_port.flit_in, pkt1.head, 1)
//       dut.clock.step(3)
//       val pkt2 = (new soft_RouteConfigPacket((0, 0), (1, 1), 8, (0, 1))).toFlits
//       enqFlit(dut.io.router_port.flit_in, pkt2.head, 0)
//       dut.clock.step(3)
//       val pkt3 = (new soft_DataPacket(5, BigInt("DEAD", 16))).toFlits
//       enqFlit(dut.io.scheduler_port.flit_in, pkt3.head, 0)
//       dut.clock.step(3)
//       val pkt4 = (new soft_DataPacket(8, BigInt("BED", 16))).toFlits
//       enqFlit(dut.io.scheduler_port.flit_in, pkt4.head, 0)
//       dut.clock.step(10)
//     }
//   }
// }

class NetworkWithNISpec extends AnyFreeSpec with ChiselScalatestTester {
  "NetworkWithNI" in {
    test(new NetworkExampleWithNI(true)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(50000)
      def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
        port.valid.poke(true)
        helper.pokeFlit(port, flit, vc_id)
        dut.clock.step()
        port.valid.poke(false)
      }
        def enqFlituser(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int,ID:Int,priority:Int,currPackagelength:Int,tagPackagelength:Int) = {
        port.valid.poke(true)
        helper.userpokeFlit(port, flit, vc_id,ID,priority,currPackagelength,tagPackagelength)
        dut.clock.step()
        port.valid.poke(false)
      }
      import helper._
      implicit val clk = dut.clock
      implicit val d = dut

      def task(vc_id: Int,priority:Int,ID:Int,task_id:Int) ={
        val flit1s = new TestPacket(1,(0,0),(0,1),3,task_id,
        load=Array(BigInt("0",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
        val flit2s = new TestPacket(1,(0,0),(1,1),3,task_id,
        load=Array(BigInt("0",2),BigInt("11111111111111111111111222222222",16),BigInt("33333333333333333344444444444444",16))).toFlits  
        val flit3s = new TestPacket(1,(0,0),(0,2),3,task_id,
        load=Array(BigInt("0",2),BigInt("33333333333333333444444444444444",16),BigInt("55555555555555555566666666666666",16))).toFlits     

      //vc_id: Int, ID: Int, priority: Int, currPackagelength: Int, tagPackagelength: Int
        enqFlituser(dut.io.local00.flit_in, flit1s(0), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit1s(1), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit1s(2), vc_id,ID,priority,3,3)

        enqFlituser(dut.io.local00.flit_in, flit2s(0), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit2s(1), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit2s(2), vc_id,ID,priority,3,3)

        enqFlituser(dut.io.local00.flit_in, flit3s(0), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit3s(1), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit3s(2), vc_id,ID,priority,3,3)

      }

        dut.clock.step(5)    
        var pkt = (new soft_RouteConfigPacket((0, 0), (0,1), 1, (1, 0))).toFlits    //sm4运行结束后将数据转发给sm3
        enqFlit(dut.io.local00.flit_in, pkt.head, 0) 
        var pkt1 = (new soft_RouteConfigPacket((0, 0), (1,0),1, (1, 1))).toFlits   //sm3运行结束后将数据转发sm3——512
        enqFlit(dut.io.local00.flit_in, pkt1.head, 0)  
        var pkt2 = (new soft_RouteConfigPacket((0, 0), (1,1),1, (1, 2))).toFlits  //sm3_512运行结束后 ，将数据转发给路由节点空点
        enqFlit(dut.io.local00.flit_in, pkt2.head, 0)  
        var pkt3 = (new soft_RouteConfigPacket((0, 0), (0,2),1, (1, 2))).toFlits  //potmul运行结束后 ，将数据转发给路由节空点
        enqFlit(dut.io.local00.flit_in, pkt3.head, 0)  

        task(0,0,7,1)
        task(1,1,6,1)
        task(2,2,5,1)
        task(3,3,4,1)
        dut.clock.step(2650)  
 
    }
  }
}


class NetworkWithNISpec1 extends AnyFreeSpec with ChiselScalatestTester {
  "NetworkWithNI" in {
    test(new NetworkExampleWithNI(true)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(1000000)
      def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
        port.valid.poke(true)
        helper.pokeFlit(port, flit, vc_id)
        dut.clock.step()
        port.valid.poke(false)
      }
        def enqFlituser(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int,ID:Int,priority:Int,currPackagelength:Int,tagPackagelength:Int) = {
        port.valid.poke(true)
        helper.userpokeFlit(port, flit, vc_id,ID,priority,currPackagelength,tagPackagelength)
        dut.clock.step()
        port.valid.poke(false)
      }


      import helper._
      implicit val clk = dut.clock
      implicit val d = dut

      def task(vc_id: Int,priority:Int,ID:Int,task_id:Int) ={
      val flit1s = new TestPacket(1,(0,0),(0,1),3,task_id,  //sm4
      load=Array(BigInt("0",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
      val flit2s = new TestPacket(1,(0,0),(1,1),3,task_id,  //sm3512
      load=Array(BigInt("0",2),BigInt("11111111111111111111111222222222",16),BigInt("33333333333333333344444444444444",16))).toFlits  
      val flit3s = new TestPacket(1,(0,0),(0,2),3,task_id,  //potmul
      load=Array(BigInt("0",2),BigInt("33333333333333333444444444444444",16),BigInt("55555555555555555566666666666666",16))).toFlits     
      val flit4s = new TestPacket(1,(0,0),(2,0),3,task_id,  //potmul
      load=Array(BigInt("0",2),BigInt("66666666666666666666666666666666",16),BigInt("77777777777777777777777777777777",16))).toFlits   
      dut.clock.step(5)    

      //vc_id: Int, ID: Int, priority: Int, currPackagelength: Int, tagPackagelength: Int

        enqFlituser(dut.io.local00.flit_in, flit2s(0), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit2s(1), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit2s(2), vc_id,ID,priority,3,3)

        enqFlituser(dut.io.local00.flit_in, flit1s(0), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit1s(1), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit1s(2), vc_id,ID,priority,3,3)

        enqFlituser(dut.io.local00.flit_in, flit3s(0), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit3s(1), vc_id,ID,priority,3,3)
        enqFlituser(dut.io.local00.flit_in, flit3s(2), vc_id,ID,priority,3,3)
      }

      def routerconfig(task_id:Int)={
        var pkt = (new soft_RouteConfigPacket((0, 0), (0,1), task_id, (1, 0))).toFlits    //sm4运行结束后将数据转发给sm3
        enqFlit(dut.io.local00.flit_in, pkt.head, 0) 
        var pkt1 = (new soft_RouteConfigPacket((0, 0), (1,0), task_id, (1, 1))).toFlits   //sm3运行结束后将数据转发sm3——512
        enqFlit(dut.io.local00.flit_in, pkt1.head, 0)  
        var pkt2 = (new soft_RouteConfigPacket((0, 0), (1,1), task_id, (1,2))).toFlits  //sm3_512运行结束后 ，将数据转发给路由节点乘空点
        enqFlit(dut.io.local00.flit_in, pkt2.head, 0)  
        var pkt3 = (new soft_RouteConfigPacket((0, 0), (0,2), task_id, (1,2))).toFlits  //potmul运行结束后 ，将数据转发给路由节空点
        enqFlit(dut.io.local00.flit_in, pkt3.head, 0)  
        dut.clock.step(5)
      }
      routerconfig(1)
      routerconfig(2)
      task(1,1,0,1)
      task(1,1,1,1)
      task(1,1,2,1)
      task(1,1,3,1)
      dut.clock.step(10000)     
      task(1,1,0,2)
      task(1,1,1,2)
      task(1,1,2,2)
      task(1,1,3,2)
      dut.clock.step(10000)
    }
  }
}


class NetworkWithNISpec_test extends AnyFreeSpec with ChiselScalatestTester {
  "NetworkWithNI" in {
    test(new NetworkExampleWithNI(true)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(1)
      def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
        port.valid.poke(true)
        helper.pokeFlit(port, flit, vc_id)
        dut.clock.step()
        port.valid.poke(false)
      }
        def enqFlituser(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int,ID:Int,priority:Int,currPackagelength:Int,tagPackagelength:Int) = {
        port.valid.poke(true)
        helper.userpokeFlit(port, flit, vc_id,ID,priority,currPackagelength,tagPackagelength)
        dut.clock.step()
        port.valid.poke(false)
      }
      import helper._
      implicit val clk = dut.clock
      implicit val d = dut

      //1.路由配置
        dut.clock.step(5)    
        var pkt = (new soft_RouteConfigPacket((0, 0), (0,1), 1, (1, 0))).toFlits    //sm4运行结束后将数据转发给sm3
        enqFlit(dut.io.local00.flit_in, pkt.head, 0) 
        var pkt1 = (new soft_RouteConfigPacket((0, 0), (1,0),1, (1, 1))).toFlits   //sm3运行结束后将数据转发sm3——512
        enqFlit(dut.io.local00.flit_in, pkt1.head, 0)  
        var pkt2 = (new soft_RouteConfigPacket((0, 0), (1,1),1, (1, 2))).toFlits  //sm3_512运行结束后 ，将数据转发给路由节点空点
        enqFlit(dut.io.local00.flit_in, pkt2.head, 0)  
        var pkt3 = (new soft_RouteConfigPacket((0, 0), (0,2),1, (1, 2))).toFlits  //potmul运行结束后 ，将数据转发给路由节空点
        enqFlit(dut.io.local00.flit_in, pkt3.head, 0)  

      //2.数据生成
        /* 创建三个队列来存储数据
           定义一个包含优先级的元组，元组的第一个元素是优先级，第二个元素是数据
           创建具有优先级的 PriorityQueue */
        type PriorityQueueElement = (Int, List[soft_Flit])
        val prioritySm4Queue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
        val prioritySm3Queue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
        val prioritypotQueue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
        val prioritymulivQueue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))

        def userdata(vc_id: Int,priority:Int,ID:Int,task_id:Int) : List[List[soft_Flit]]= {
          val sm4_data = new TestPacket(1,(0,0),(0,1),3,task_id,  //sm4
          load=Array(BigInt("0",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
          val sm3_data = new TestPacket(1,(0,0),(1,1),3,task_id, //sm3_512
          load=Array(BigInt("0",2),BigInt("11111111111111111111111222222222",16),BigInt("33333333333333333344444444444444",16))).toFlits  
          val potmul = new TestPacket(1,(0,0),(0,2),3,task_id, //点乘
          load=Array(BigInt("0",2),BigInt("33333333333333333444444444444444",16),BigInt("55555555555555555566666666666666",16))).toFlits 
          val muliv = new TestPacket(1,(0,0),(2,0),3,task_id, //模逆
          load=Array(BigInt("0",2),BigInt("66666666666666666666666666666666",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits   
          // 将数据入队到相应的队列
          prioritySm4Queue.enqueue((priority, sm4_data))
          prioritySm3Queue.enqueue((priority, sm3_data))
          prioritypotQueue.enqueue((priority, potmul))
          prioritymulivQueue.enqueue((priority,muliv))
          List(sm4_data,sm4_data,potmul,muliv)   
      }
      val user0 = userdata(0,0,0,1)
      val user1 = userdata(1,1,1,1)
      val user2 = userdata(2,2,2,1)
      val user3 = userdata(3,3,3,1)
      val user4 = userdata(0,0,4,1)
      val user5 = userdata(1,1,5,1)
      val user6 = userdata(1,1,6,1)
      val user7 = userdata(1,1,7,1)

          var sm4pagenum = 0
          var sm3pagenum = 0  
          var potpagenum = 0   
          var mulivpagenum = 0 
          var mulivresources = 4   
          var sm4resources =4        
          var sm3resources =4  
          var potresources =4
          while((sm4pagenum<8) || (sm3pagenum<8)||(potpagenum<8)||(mulivpagenum<8)){
            while(sm4resources>0 && dut.io.runned01.peekInt()<4){
              if(!prioritySm4Queue.isEmpty){
                //输入sm4
                val (priority, task) = prioritySm4Queue.dequeue()
                enqFlituser(dut.io.local00.flit_in,task(0),1,sm4pagenum,priority,3,3)
                enqFlituser(dut.io.local00.flit_in,task(1),1,sm4pagenum,priority,3,3)
                enqFlituser(dut.io.local00.flit_in,task(2),1,sm4pagenum,priority,3,3)
                sm4resources = sm4resources -1
                sm4pagenum = sm4pagenum + 1
                println(s"Priority: ${priority},dut.io.runCount.peekInt:${dut.io.runned01.peekInt().toInt},sm4pagenum:${sm4pagenum},sm4resources:${sm4resources}")
                dut.clock.step(3)
              }

            }
            if(prioritySm4Queue.isEmpty){
              sm4resources=0
            }
          while(sm3resources>0 && dut.io.sm311tcpcount.peekInt()<3){
            if(!prioritySm3Queue.isEmpty){
              //输入sm3
              val (priority, task) = prioritySm3Queue.dequeue()
              enqFlituser(dut.io.local00.flit_in,task(0),1,sm3pagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(1),1,sm3pagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(2),1,sm3pagenum,priority,3,3)   
              sm3resources = sm3resources -1
              sm3pagenum = sm3pagenum + 1
              println(s"Priority: $priority, Task: ${task.head},dut.io.runCount.peekInt:${dut.io.sm311tcpcount.peekInt().toInt},sm3pagenum:${sm3pagenum},sm3resources:${sm3resources}")
            }
          }
          if(prioritySm3Queue.isEmpty){
            sm3resources=0
          }
          while(mulivresources>0 && dut.io.runned20.peekInt()<4){
            if(!prioritymulivQueue.isEmpty){
              //输入pot
              val (priority, task) = prioritymulivQueue.dequeue()
              enqFlituser(dut.io.local00.flit_in,task(0),1,mulivpagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(1),1,mulivpagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(2),1,mulivpagenum,priority,3,3)   
              mulivresources = mulivresources -1
              mulivpagenum = mulivpagenum + 1
              println(s"Priority: $priority,dut.io.runned20.peekInt:${dut.io.runned20.peekInt().toInt},mulivpagenum:${mulivpagenum},mulivresources:${mulivresources}")
            }
          }
          if(prioritymulivQueue.isEmpty){
            mulivresources=0
          }

          while(potresources>0 && dut.io.runned02.peekInt()<4){
            if(!prioritypotQueue.isEmpty){
              //输入pot
              val (priority, task) = prioritypotQueue.dequeue()
              enqFlituser(dut.io.local00.flit_in,task(0),1,potpagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(1),1,potpagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(2),1,potpagenum,priority,3,3)   
              dut.clock.step(5)
              potresources = potresources -1
              potpagenum = potpagenum + 1
              println(s"Priority: $priority,dut.io.potrunCount.peekInt:${dut.io.runned02.peekInt().toInt},potpagenum:${potpagenum},potresources:${potresources}")
            }
          }
          if(prioritypotQueue.isEmpty){
            potresources=0
          }
            if((sm4resources === 0) && (sm3resources === 0)&&(potresources === 0)&&(mulivresources === 0)) {
                if (dut.io.runned01.peekInt() < 4 || dut.io.sm311tcpcount.peekInt() < 4 ||dut.io.runned02.peekInt()<4||dut.io.runned20.peekInt()<4) {
                  if (dut.io.runned01.peekInt() <4&&(!prioritySm4Queue.isEmpty)){
                      sm4resources +=1 // 或者根据需要重置的资源数量
                      println(s"dut.io.runned01.peekInt():${dut.io.runned01.peekInt()}")
                  }
                  if (dut.io.sm311tcpcount.peekInt() < 4 && (!prioritySm3Queue.isEmpty)) {
                    sm3resources += 1 // 或者根据需要重置的资源数量
                    println(s"sm3pagenum:${sm3pagenum} sm3resources:${sm3resources} ")
                  }
                  if(dut.io.runned20.peekInt()<4 && (!prioritymulivQueue.isEmpty)){
                    mulivresources+=1
                    println(s"mulivpagenum:${mulivpagenum} mulivresources:${mulivresources} ")     
                  }
                  if(dut.io.runned02.peekInt()<4 && (!prioritypotQueue.isEmpty)){
                    potresources+=1
                    println(s"potpagenum:${potpagenum} potresources:${potresources} ")     
                  }
                }
                dut.clock.step(5)
                println(s"是否卡住 sm4pagenum:${sm4pagenum} sm3pagenum:${sm3pagenum},sm4pagenum:${sm3pagenum}")
            }
            println(s"是否外部卡住,sm4pagenum:${sm4pagenum} sm3pagenum:${sm3pagenum} potpagenum:${potpagenum}  sm4resources:${sm4resources} sm3resources :${sm3resources}  potresources :${potresources}")
            println(s"是否外部卡住,dut.io.runned01.peekInt():${dut.io.runned01.peekInt()} dut.io.sm311tcpcount.peekInt():${dut.io.sm311tcpcount.peekInt()} dut.io.runned02:${dut.io.runned02.peekInt()}")
            dut.clock.step()
          }
        println(s"结束while循环 sm4pagenum:${sm4pagenum} sm3pagenum:${sm3pagenum},sm4pagenum:${sm3pagenum},mulivpagenum:${mulivpagenum} ")
        println(s"时间检测 sm4resources:${sm4resources}  sm3resources:${sm3resources}  potresources:${potresources} mulivresources:${mulivresources} ")
        dut.clock.step(200000)

    }
  }
}



class NetworkWithNISpec_test2 extends AnyFreeSpec with ChiselScalatestTester {
  "NetworkWithNI" in {
    test(new NetworkExampleWithNI(true)).withAnnotations(Seq(VerilatorBackendAnnotation, WriteVcdAnnotation)) { dut =>
      dut.clock.setTimeout(1000000)
      def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
        port.valid.poke(true)
        helper.pokeFlit(port, flit, vc_id)
        dut.clock.step()
        port.valid.poke(false)
      }
        def enqFlituser(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int,ID:Int,priority:Int,currPackagelength:Int,tagPackagelength:Int) = {
        port.valid.poke(true)
        helper.userpokeFlit(port, flit, vc_id,ID,priority,currPackagelength,tagPackagelength)
        dut.clock.step()
        port.valid.poke(false)
      }
      import helper._
      implicit val clk = dut.clock
      implicit val d = dut

      //1.路由配置
        dut.clock.step(5)    
        var pkt = (new soft_RouteConfigPacket((0, 0), (0,1), 1, (1, 0))).toFlits    //sm4运行结束后将数据转发给sm3
        enqFlit(dut.io.local00.flit_in, pkt.head, 0) 
        var pkt1 = (new soft_RouteConfigPacket((0, 0), (1,0),1, (1, 1))).toFlits   //sm3运行结束后将数据转发sm3——512
        enqFlit(dut.io.local00.flit_in, pkt1.head, 0)  
        var pkt2 = (new soft_RouteConfigPacket((0, 0), (1,1),1, (1, 2))).toFlits  //sm3_512运行结束后 ，将数据转发给路由节点空点
        enqFlit(dut.io.local00.flit_in, pkt2.head, 0)  
        var pkt3 = (new soft_RouteConfigPacket((0, 0), (0,2),1, (1, 2))).toFlits  //potmul运行结束后 ，将数据转发给路由节空点
        enqFlit(dut.io.local00.flit_in, pkt3.head, 0)  

      //2.数据生成
        /* 创建三个队列来存储数据
           定义一个包含优先级的元组，元组的第一个元素是优先级，第二个元素是数据
           创建具有优先级的 PriorityQueue */
        type PriorityQueueElement = (Int, List[soft_Flit])
        val prioritySm4Queue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
        val prioritySm3Queue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
        val prioritypotQueue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
        val prioritymulivQueue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))

        def userdata(vc_id: Int,priority:Int,ID:Int,task_id:Int) : List[List[soft_Flit]]= {
          val sm4_data = new TestPacket(1,(0,0),(0,1),3,task_id,  //sm4
          load=Array(BigInt("0",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
          val sm3_data = new TestPacket(1,(0,0),(1,1),3,task_id, //sm3_512
          load=Array(BigInt("0",2),BigInt("11111111111111111111111222222222",16),BigInt("33333333333333333344444444444444",16))).toFlits  
          val potmul = new TestPacket(1,(0,0),(0,2),3,task_id, //点乘
          load=Array(BigInt("0",2),BigInt("33333333333333333444444444444444",16),BigInt("55555555555555555566666666666666",16))).toFlits 
          val muliv = new TestPacket(1,(0,0),(2,0),3,task_id, //模逆
          load=Array(BigInt("0",2),BigInt("66666666666666666666666666666666",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits   
          // 将数据入队到相应的队列
          prioritySm4Queue.enqueue((priority, sm4_data))
          prioritySm3Queue.enqueue((priority, sm3_data))
          prioritypotQueue.enqueue((priority, potmul))
          prioritymulivQueue.enqueue((priority,muliv))
          List(sm4_data,sm4_data,potmul,muliv)   
      }
      val user0 = userdata(0,0,0,1)
      val user1 = userdata(1,1,1,1)
      val user2 = userdata(2,2,2,1)
      val user3 = userdata(3,3,3,1)
      val user4 = userdata(0,0,4,1)
      val user5 = userdata(1,1,5,1)
      val user6 = userdata(1,1,6,1)
      val user7 = userdata(1,1,7,1)

          var sm4pagenum = 0
          var sm3pagenum = 0  
          var potpagenum = 0   
          var mulivpagenum = 0 
          var mulivresources = 4   
          var sm4resources =4        
          var sm3resources =4  
          var potresources =4
          while((sm4pagenum<8) || (sm3pagenum<8)||(potpagenum<8)||(mulivpagenum<8)){
            if(sm4resources>0 && dut.io.runned01.peekInt()<4){
              if(!prioritySm4Queue.isEmpty){
                //输入sm4
                val (priority, task) = prioritySm4Queue.dequeue()
                enqFlituser(dut.io.local00.flit_in,task(0),1,sm4pagenum,priority,3,3)
                enqFlituser(dut.io.local00.flit_in,task(1),1,sm4pagenum,priority,3,3)
                enqFlituser(dut.io.local00.flit_in,task(2),1,sm4pagenum,priority,3,3)
                sm4resources = sm4resources -1
                sm4pagenum = sm4pagenum + 1
                println(s"Priority: ${priority},dut.io.runCount.peekInt:${dut.io.runned01.peekInt().toInt},sm4pagenum:${sm4pagenum},sm4resources:${sm4resources}")
                dut.clock.step(5)
              }

            }
            if(prioritySm4Queue.isEmpty){
              sm4resources=0
            }
          if(sm3resources>0 && dut.io.sm311tcpcount.peekInt()<4){
            if(!prioritySm3Queue.isEmpty){
              //输入sm3
              val (priority, task) = prioritySm3Queue.dequeue()
              enqFlituser(dut.io.local00.flit_in,task(0),1,sm3pagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(1),1,sm3pagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(2),1,sm3pagenum,priority,3,3)   
              sm3resources = sm3resources -1
              sm3pagenum = sm3pagenum + 1
              println(s"Priority: $priority, Task: ${task.head},dut.io.runCount.peekInt:${dut.io.sm311tcpcount.peekInt().toInt},sm3pagenum:${sm3pagenum},sm3resources:${sm3resources}")
              dut.clock.step(5)
            }
          }
          if(prioritySm3Queue.isEmpty){
            sm3resources=0
          }
          if(mulivresources>0 && dut.io.runned20.peekInt()<4){
            if(!prioritymulivQueue.isEmpty){
              //输入pot
              val (priority, task) = prioritymulivQueue.dequeue()
              enqFlituser(dut.io.local00.flit_in,task(0),1,mulivpagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(1),1,mulivpagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(2),1,mulivpagenum,priority,3,3)   
              mulivresources = mulivresources -1
              mulivpagenum = mulivpagenum + 1
              println(s"Priority: $priority,dut.io.runned20.peekInt:${dut.io.runned20.peekInt().toInt},mulivpagenum:${mulivpagenum},mulivresources:${mulivresources}")
              dut.clock.step(5)
            }
          }
          if(prioritymulivQueue.isEmpty){
            mulivresources=0
          }

          if(potresources>0 && dut.io.runned02.peekInt()<4){
            if(!prioritypotQueue.isEmpty){
              //输入pot
              val (priority, task) = prioritypotQueue.dequeue()
              enqFlituser(dut.io.local00.flit_in,task(0),1,potpagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(1),1,potpagenum,priority,3,3)
              enqFlituser(dut.io.local00.flit_in,task(2),1,potpagenum,priority,3,3)   
              dut.clock.step(5)
              potresources = potresources -1
              potpagenum = potpagenum + 1
              println(s"Priority: $priority,dut.io.potrunCount.peekInt:${dut.io.runned02.peekInt().toInt},potpagenum:${potpagenum},potresources:${potresources}")
            }
          }
          if(prioritypotQueue.isEmpty){
            potresources=0
          }
            if((sm4resources === 0)&&(dut.io.runned01.peekInt() < 4)&&(!prioritySm4Queue.isEmpty)){
              sm4resources +=1
              println(s"dut.io.runned01.peekInt():${dut.io.runned01.peekInt()}")
            }
            if((sm3resources === 0)&&(dut.io.sm311tcpcount.peekInt() < 4)&& (!prioritySm3Queue.isEmpty)){
              sm3resources += 1 
              println(s"sm3pagenum:${sm3pagenum} dut.io.sm311tcpcount.peekInt():${dut.io.sm311tcpcount.peekInt()} ")
            }
            if((potresources === 0)&&(dut.io.runned02.peekInt()<4)&&(!prioritypotQueue.isEmpty)){
              potresources+=1
              println(s"potpagenum:${potpagenum} dut.io.runned02.peekInt():${dut.io.runned02.peekInt()} ")  
            }
            if((mulivresources === 0)&&(dut.io.runned20.peekInt()<4)&&(!prioritymulivQueue.isEmpty)){
               mulivresources+=1
               println(s"mulivpagenum:${mulivpagenum} dut.io.runned20.peekInt():${dut.io.runned20.peekInt()} ") 
            }
            println(s"是否外部卡住,dut.io.runned01.peekInt():${dut.io.runned01.peekInt()} dut.io.sm311tcpcount.peekInt():${dut.io.sm311tcpcount.peekInt()} dut.io.runned02:${dut.io.runned02.peekInt()}")
            dut.clock.step()
          }
        println(s"结束while循环 sm4pagenum:${sm4pagenum} sm3pagenum:${sm3pagenum},sm4pagenum:${sm3pagenum},mulivpagenum:${mulivpagenum} ")
        println(s"时间检测 sm4resources:${sm4resources}  sm3resources:${sm3resources}  potresources:${potresources} mulivresources:${mulivresources} ")
        dut.clock.step(900000)
    }
  }
}










































class test extends AnyFreeSpec with ChiselScalatestTester {
  "Montgomery Multiplier should multiply" /* taggedAs RequiresVerilator */ in {
    test(new MontgomeryMul(256)).withAnnotations(Seq(/* VerilatorBackendAnnotation,  */WriteVcdAnnotation)) { dut =>
      import helper._


      // 创建三个队列来存储数据
      // 定义一个包含优先级的元组，元组的第一个元素是优先级，第二个元素是数据
        type PriorityQueueElement = (Int, List[soft_Flit])
        // 创建具有优先级的 PriorityQueue
        val prioritySm4Queue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
        val prioritySm3Queue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
        val prioritypotQueue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))
      //2.数据生成
        def userdata(vc_id: Int,priority:Int,ID:Int,task_id:Int) : List[List[soft_Flit]]= {
          val sm4_data = new TestPacket(1,(0,0),(0,1),3,task_id,  //sm4
          load=Array(BigInt("0",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
          val sm3_data = new TestPacket(1,(0,0),(1,1),3,task_id, //sm3_512
          load=Array(BigInt("0",2),BigInt("11111111111111111111111222222222",16),BigInt("33333333333333333344444444444444",16))).toFlits  
          val potmul = new TestPacket(1,(0,0),(0,2),3,task_id, //点乘
          load=Array(BigInt("0",2),BigInt("33333333333333333444444444444444",16),BigInt("55555555555555555566666666666666",16))).toFlits  
          // 将数据入队到相应的队列
          prioritySm4Queue.enqueue((priority, sm4_data))
          prioritySm3Queue.enqueue((priority, sm3_data))
          prioritypotQueue.enqueue((priority, potmul))
          List(sm4_data,sm4_data,potmul)   
      }

      val user0 = userdata(0,0,0,1)
      val user1 = userdata(1,1,1,1)
      val user2 = userdata(2,2,2,1)
      val user4 = userdata(3,3,3,1)
      val user5 = userdata(0,0,4,1)
      val user6 = userdata(1,1,5,1)
      val user7 = userdata(1,1,6,1)
      while (prioritySm4Queue.nonEmpty) {
        val (priority, task) = prioritySm4Queue.dequeue()
        println(s"Priority: $priority, Task: ${task.head}")
      }

      while (prioritySm3Queue.nonEmpty) {
        val (priority, task) = prioritySm3Queue.dequeue()
        println(s"Priority: $priority, Task: ${task.head}")
      }
      while (prioritypotQueue.nonEmpty) {
        val (priority, task) = prioritypotQueue.dequeue()
        println(s"Priority: $priority, Task: ${task.head}")
      }
    }
  }
}


//           var sm4pagenum = 0
//           var sm3pagenum = 0  
//           var potpagenum = 0    
//           var sm4resources =4        
//           var sm3resources =4  
//           var potresources =4
//           while((sm4pagenum<8) || (sm3pagenum<8)||(potpagenum<8)){
//             if(sm4resources>0 && dut.io.runned01.peekInt()<4){
//               if(!prioritySm4Queue.isEmpty){
//                 //输入sm4
//                 val (priority, task) = prioritySm4Queue.dequeue()
//                 enqFlituser(dut.io.local00.flit_in,task(0),1,sm4pagenum,priority,3,3)
//                 enqFlituser(dut.io.local00.flit_in,task(1),1,sm4pagenum,priority,3,3)
//                 enqFlituser(dut.io.local00.flit_in,task(2),1,sm4pagenum,priority,3,3)
//                 sm4resources = sm4resources -1
//                 sm4pagenum = sm4pagenum + 1
//                 println(s"Priority: ${priority},dut.io.runCount.peekInt:${dut.io.runned01.peekInt().toInt},sm4pagenum:${sm4pagenum},sm4resources:${sm4resources}")
//                 dut.clock.step(3)
//               }

//             }
//             if(prioritySm4Queue.isEmpty){
//               sm4resources=0
//             }
//           if(sm3resources>0 && dut.io.sm311tcpcount.peekInt()<3){
//             if(!prioritySm3Queue.isEmpty){
//               //输入sm3
//               val (priority, task) = prioritySm3Queue.dequeue()
//               enqFlituser(dut.io.local00.flit_in,task(0),1,sm3pagenum,priority,3,3)
//               enqFlituser(dut.io.local00.flit_in,task(1),1,sm3pagenum,priority,3,3)
//               enqFlituser(dut.io.local00.flit_in,task(2),1,sm3pagenum,priority,3,3)   
//               sm3resources = sm3resources -1
//               sm3pagenum = sm3pagenum + 1
//               println(s"Priority: $priority, Task: ${task.head},dut.io.runCount.peekInt:${dut.io.sm311tcpcount.peekInt().toInt},sm3pagenum:${sm3pagenum},sm3resources:${sm3resources}")
//             }
//           }
//           if(prioritySm3Queue.isEmpty){
//             sm3resources=0
//           }
//           if(potresources>0 && dut.io.runned02.peekInt()<3){
//             if(!prioritypotQueue.isEmpty){
//               //输入pot
//               val (priority, task) = prioritySm3Queue.dequeue()
//               enqFlituser(dut.io.local00.flit_in,task(0),1,potpagenum,priority,3,3)
//               enqFlituser(dut.io.local00.flit_in,task(1),1,potpagenum,priority,3,3)
//               enqFlituser(dut.io.local00.flit_in,task(2),1,potpagenum,priority,3,3)   
//               potresources = potresources -1
//               potpagenum = potpagenum + 1
//               println(s"Priority: $priority,dut.io.potrunCount.peekInt:${dut.io.runned02.peekInt().toInt},potpagenum:${potpagenum},potresources:${potresources}")
//             }
//           }
//           if(prioritypotQueue.isEmpty){
//             potresources=0
//           }
//             while((sm4resources === 0) && (sm3resources === 0)&&(potresources === 0)) {
//                 if (dut.io.runned01.peekInt() <= 3 || dut.io.sm311tcpcount.peekInt() <= 3 ||dut.io.runned02.peekInt()<=3) {
//                   if (dut.io.runned01.peekInt() <= 3&&(!prioritySm4Queue.isEmpty)){
//                       sm4resources +=1 // 或者根据需要重置的资源数量
//                       println(s"dut.io.runned01.peekInt():${dut.io.runned01.peekInt()}")
//                   }
//                   if (dut.io.sm311tcpcount.peekInt() <= 3 && (!prioritySm3Queue.isEmpty)) {
//                     sm3resources += 1 // 或者根据需要重置的资源数量
//                     println(s"sm3pagenum:${sm3pagenum} sm3resources:${sm3resources} ")
//                   }
//                   if(dut.io.runned02.peekInt()<=3 && ( (!prioritypotQueue.isEmpty))){
//                     potresources+=1
//                     println(s"potpagenum:${potpagenum} potresources:${potresources} ")     
//                   }
//                 }
//                 dut.clock.step(5)
//                 println(s"是否卡住 sm4pagenum:${sm4pagenum} sm3pagenum:${sm3pagenum},sm4pagenum:${sm3pagenum}")
//             }
//             println(s"是否外部卡住,sm4pagenum:${sm4pagenum} sm3pagenum:${sm3pagenum} potpagenum:${potpagenum}  sm4resources:${sm4resources} sm3resources :${sm3resources}  potresources :${potresources}")
//               println(s"是否外部卡住,sm4pagenum:${sm4pagenum} sm3pagenum:${sm3pagenum} potpagenum:${potpagenum}  sm4resources:${sm4resources} sm3resources :${sm3resources}  potresources :${potresources}")
//             dut.clock.step()
//           }

//         println(s"时间检测 sm4resources:${sm4resources}  sm3resources:${sm3resources}  potresources:${potresources} ")
//         dut.clock.step(200000)

//     }
//   }
// }
