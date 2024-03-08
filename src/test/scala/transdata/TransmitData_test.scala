package NetworkInterface
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import treadle.executable.Big
import org.scalatest._
import scala.util.Random
import chisel3.util._
import mesh_network._
import org.scalatest._
import NetworkInterface._
import scala.collection.mutable.PriorityQueue
import scala.concurrent.duration._
class topmodule2 extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new Top_SM4networkadaptor(4)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut => 

    }
  }
}

class topmodule3 extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new tesSM4CCnetworkadaptor(4)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut => 
        import NetworkConfig._
      dut.clock.setTimeout(2000)


        def opertor(taglength:Int,currlength:Int,ID:Int,Data:Int):String ={
          val currPackagelength = Util.wrapWithPadding0(currlength,log2Ceil(NetworkConfig.currpackagelength_width))
          val tagPackagelength = Util.wrapWithPadding0(taglength,log2Ceil(NetworkConfig.currpackagelength_width))
          val id = Util.wrapWithPadding0(ID,log2Ceil(NetworkConfig.priority_width))
          tagPackagelength+currPackagelength+id
        }

        def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int,ID:Int,priority:Int,currPackagelength:Int,tagPackagelength:Int) = {
        port.valid.poke(true)
        helper.userpokeFlit(port, flit, vc_id,ID,priority,currPackagelength,tagPackagelength)
        dut.clock.step()
        port.valid.poke(false)
      }
      
        val flit1s = new TestPacket(1,(0,0),(0,1),3,0,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  

        val flit2s = new TestPacket(1,(0,0),(0,1),3,1,
        load=Array(BigInt("0011010110",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits     

        val flit3s = new TestPacket(1,(0,0),(0,1),3,2,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  

        val flit4s = new TestPacket(1,(0,0),(0,1),3,3,
        load=Array(BigInt("0011010110",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits     

        
        //mesh_network.soft_Flit, vc_id: Int, ID: Int, priority: Int, currPackagelength: Int, tagPackagelength: Int
      dut.io.inputs(0).valid.poke(true)
      dut.io.inputs(1).valid.poke(true)
      dut.io.inputs(2).valid.poke(true)
      dut.io.inputs(3).valid.poke(true)
      helper.userpokeFlit(dut.io.inputs(0), flit1s(0),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(0),1,1,1,3,5)
      helper.userpokeFlit(dut.io.inputs(2), flit3s(0),2,2,1,3,5)
      helper.userpokeFlit(dut.io.inputs(3), flit4s(0),3,3,1,3,5)
      dut.clock.step()
      helper.userpokeFlit(dut.io.inputs(0), flit1s(1),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(1),1,1,1,3,5)
      helper.userpokeFlit(dut.io.inputs(2), flit3s(1),2,2,1,3,5)
      helper.userpokeFlit(dut.io.inputs(3), flit4s(1),3,3,1,3,5)
      dut.clock.step()
      helper.userpokeFlit(dut.io.inputs(0), flit1s(2),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(2),1,1,1,3,5)
      helper.userpokeFlit(dut.io.inputs(2), flit3s(2),2,2,1,3,5)
      helper.userpokeFlit(dut.io.inputs(3), flit4s(2),3,3,1,3,5)
      dut.clock.step()
      helper.userpokeFlit(dut.io.inputs(0), flit1s(1),0,6,1,2,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(1),1,1,1,2,5)
      helper.userpokeFlit(dut.io.inputs(2), flit3s(1),2,2,1,2,5)
      helper.userpokeFlit(dut.io.inputs(3), flit4s(1),3,3,1,2,5)
      dut.clock.step()
      helper.userpokeFlit(dut.io.inputs(0), flit1s(2),0,6,1,2,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(2),1,1,1,2,5)
      helper.userpokeFlit(dut.io.inputs(2), flit3s(2),2,2,1,2,5)
      helper.userpokeFlit(dut.io.inputs(3), flit4s(2),3,3,1,2,5)
      dut.clock.step()  
      dut.io.inputs(0).valid.poke(false)
      dut.io.inputs(1).valid.poke(false) 
      dut.io.inputs(2).valid.poke(false)
      dut.io.inputs(3).valid.poke(false)
      dut.clock.step()  


      dut.io.output.ready.poke(true)

      // 设置一个时间窗口
      val numCyclesToCheck =  500 // 假设检测10个时钟周期    
      // 循环检测模块的输出
      var cycleCount = 0
      while (cycleCount < numCyclesToCheck) {
        dut.clock.step()
        if(dut.io.runCount.peekInt()!=0){
            println(s"Cycle $cycleCount: Output data: ${dut.io.runCount.peekInt()}")
        }
        cycleCount += 1
      }
       dut.clock.step(1500)
    }
  }
}



class topmodule4 extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new Top_potCCnetworkadaptor(4)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut => 
        import NetworkConfig._
      dut.clock.setTimeout(10000)
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



      //2.数据生成
        /* 创建三个队列来存储数据
           定义一个包含优先级的元组，元组的第一个元素是优先级，第二个元素是数据
           创建具有优先级的 PriorityQueue */
        type PriorityQueueElement = (Int, List[soft_Flit])
        val prioritySm4Queue = PriorityQueue[PriorityQueueElement]()(Ordering.by(_._1))

        def userdata(vc_id: Int,priority:Int,ID:Int,task_id:Int) : List[List[soft_Flit]]= {
          val sm4_data = new TestPacket(1,(0,0),(0,1),3,task_id,  //sm4
          load=Array(BigInt("0",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
          // 将数据入队到相应的队列
          prioritySm4Queue.enqueue((priority, sm4_data))

          List(sm4_data)   
      }
      val user0 = userdata(0,0,0,1)
      val user1 = userdata(1,1,1,1)
      val user2 = userdata(2,2,2,1)
      val user3 = userdata(3,3,3,1)
      val user4 = userdata(0,0,4,1)
      val user5 = userdata(1,1,5,1)
      val user6 = userdata(1,1,6,1)
      val user7 = userdata(1,1,7,1)
          // 创建一个纳秒级别的时间间隔
        val nanosecondsInterval: FiniteDuration = 20.nanoseconds
          var sm4pagenum = 0
          var sm4resources =4        

          while((sm4pagenum<8)){
            while(sm4resources>0 && dut.io.runCount.peekInt()<4){
              if(!prioritySm4Queue.isEmpty){
                //输入sm4
                val (priority, task) = prioritySm4Queue.dequeue()
                enqFlituser(dut.io.inputs(0),task(0),1,sm4pagenum,priority,3,3)
                enqFlituser(dut.io.inputs(0),task(1),1,sm4pagenum,priority,3,3)
                enqFlituser(dut.io.inputs(0),task(2),1,sm4pagenum,priority,3,3)
                sm4resources = sm4resources -1
                sm4pagenum = sm4pagenum + 1
                println(s"Priority: ${priority},dut.io.runCount.peekInt:${dut.io.runCount.peekInt().toInt},sm4pagenum:${sm4pagenum},sm4resources:${sm4resources}")
                
              }
            }
            if(prioritySm4Queue.isEmpty){
              sm4resources=0
            }

            if((sm4resources === 0)&&(dut.io.runCount.peekInt()< 4)&&(!prioritySm4Queue.isEmpty)){
                  sm4resources +=1
                  println(s"dut.io.runned01.peekInt():${dut.io.runCount.peekInt()}")
            }
            println(s"是否外部卡住,dut.io.runned01.peekInt():${dut.io.runCount.peekInt()}")
            dut.clock.step()
          }
        println(s"结束while循环 sm4pagenum:${sm4pagenum}")
        println(s"时间检测 sm4resources:${sm4resources} ")
        dut.clock.step(2000)
    }
  }
}


class topmodule5 extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new Top_addsupnetworkadaptor(4)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut => 
        import NetworkConfig._
      dut.clock.setTimeout(250000)
        def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int,ID:Int,priority:Int,currPackagelength:Int,tagPackagelength:Int) = {
        port.valid.poke(true)
        helper.userpokeFlit(port, flit, vc_id,ID,priority,currPackagelength,tagPackagelength)
        dut.clock.step()
        port.valid.poke(false)
      }
        val flit1s = new TestPacket(1,(0,0),(0,1),3,1,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
        val flit2s = new TestPacket(1,(0,0),(0,1),3,2,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  

         val flit3s = new TestPacket(1,(0,0),(0,1),3,1,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
        val flit4s = new TestPacket(1,(0,0),(0,1),3,2,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  


      dut.io.inputs(0).valid.poke(true)
      dut.io.inputs(1).valid.poke(true)
      helper.userpokeFlit(dut.io.inputs(0), flit1s(0),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(0),1,7,1,3,5)
      dut.clock.step()
      helper.userpokeFlit(dut.io.inputs(0), flit1s(1), 0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(1), 1,7,1,3,5)
      dut.clock.step()   
      helper.userpokeFlit(dut.io.inputs(0), flit1s(2),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(2),1,7,1,3,5)
      dut.clock.step()

      dut.io.inputs(0).valid.poke(false)
      dut.io.inputs(1).valid.poke(false)
      dut.clock.step(100)
      dut.io.inputs(0).valid.poke(true)
      dut.io.inputs(1).valid.poke(true)
      helper.userpokeFlit(dut.io.inputs(0), flit3s(0),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit4s(0),1,7,1,3,5)
      dut.clock.step()
      helper.userpokeFlit(dut.io.inputs(0), flit3s(1), 0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit4s(1), 1,7,1,3,5)
      dut.clock.step()   
      helper.userpokeFlit(dut.io.inputs(0), flit3s(2),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit4s(2),1,7,1,3,5)
      dut.clock.step()
      dut.io.inputs(0).valid.poke(false)
      dut.io.inputs(1).valid.poke(false) 
      dut.clock.step()  

      dut.io.output.ready.poke(true)
      dut.clock.step(110000)
      
    }
  }
}


class top_muliv extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new Top_mulivnetworkadaptor(4)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut => 
        import NetworkConfig._
      dut.clock.setTimeout(250000)
        def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int,ID:Int,priority:Int,currPackagelength:Int,tagPackagelength:Int) = {
        port.valid.poke(true)
        helper.userpokeFlit(port, flit, vc_id,ID,priority,currPackagelength,tagPackagelength)
        dut.clock.step()
        port.valid.poke(false)
      }
        val flit1s = new TestPacket(1,(0,0),(0,1),3,1,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
        val flit2s = new TestPacket(1,(0,0),(0,1),3,2,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  

         val flit3s = new TestPacket(1,(0,0),(0,1),3,1,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
        val flit4s = new TestPacket(1,(0,0),(0,1),3,2,
        load=Array(BigInt("0011010101",2),BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  


      dut.io.inputs(0).valid.poke(true)
      dut.io.inputs(1).valid.poke(true)
      helper.userpokeFlit(dut.io.inputs(0), flit1s(0),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(0),1,7,1,3,5)
      dut.clock.step()
      helper.userpokeFlit(dut.io.inputs(0), flit1s(1), 0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(1), 1,7,1,3,5)
      dut.clock.step()   
      helper.userpokeFlit(dut.io.inputs(0), flit1s(2),0,6,1,3,5)
      helper.userpokeFlit(dut.io.inputs(1), flit2s(2),1,7,1,3,5)
      dut.clock.step()
      dut.io.inputs(0).valid.poke(false)
      dut.io.inputs(1).valid.poke(false) 
      dut.clock.step()  

      dut.io.output.ready.poke(true)
      dut.clock.step(110000)
      
    }
  }
}

