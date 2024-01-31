package NetworkInterface
import mesh_network._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import _root_.operator.AES
import os.truncate
import NetworkInterfaceArbiter._
import clusterSM4._
import java.io.DataInput
import  Interline._
import scala.util.Random

class test_transmitDatapotaddsup(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends  Module {
    val io = IO(new test_InterfaceMULBundle)
     /*
    InterfaceDataInrdyReg:寄存器与输入io的ready信号连接
    InterfaceDataOutvldReg:寄存器与输入io的ready信号连接
    DataInRdyReg : 寄存器与输入io的ready信号连接
    DataOutVldReg ： 寄存器与输出io的valid 信号连接
    */ 
    val InterfaceDataInrdyReg = RegInit(true.B)
    val InterfaceDataOutvldReg=RegInit(false.B)

    val InterfaceuserinrdyReg = RegInit(true.B) 
    val InterfaceuseroutvldReg = RegInit(false.B)  //将user信息传送给调度模块


    val runnedReg = RegInit(false.B)

    io.NetworkInterfaceDataIN.ready:=InterfaceDataInrdyReg
    io.NetworkInterfaceDataOUT.valid:=InterfaceDataOutvldReg

    io.NetworkInterfaceuserIn.ready:=InterfaceuserinrdyReg
    io.NetworkInterfaceuserOUT.valid:=InterfaceuseroutvldReg
 


    //PCB表
    val userInfomationRegs =RegInit(VecInit.fill(8)(0.U.asTypeOf(new PcbReg)))

   
    //用户数据存储器
    val usermemoryqueues = Seq.fill(8)(Module(new Queue(new Flit, 16)))
    val usermemeoryqueues_ports = Wire(Vec(8, chiselTypeOf(usermemoryqueues(0).io)))
    usermemeoryqueues_ports.zip(usermemoryqueues).foreach{case(m, n) =>
        m<>n.io
    }
    val userportsrdys=RegInit(VecInit.fill(8)(false.B))
    for(i<-0 until 8){
        usermemeoryqueues_ports(i).enq.valid:=false.B
        usermemeoryqueues_ports(i).enq.bits:=DontCare
        usermemeoryqueues_ports(i).deq.ready:=userportsrdys(i)

        when(usermemeoryqueues_ports(i).deq.fire){
            userportsrdys(i):=false.B
        }
    }
    //缓冲配置数据
    val configReg = RegInit((new configlenRegBundle).Lit(_.data_len -> datalen.U,
                                                         _.key_len -> keylen.U,
                                                         _.cfg_len ->cfglen.U,
                                                         _.result_len->resultlen.U))


    //输出数据寄存器
    val DataOutReg = RegInit(0.U.asTypeOf(new test_UnifyfieldReg))

    //输出结果寄存器
    val DataInReg = RegInit(VecInit.fill(3)(0.U.asTypeOf(new Flit)))

    
    val resultlentReg = RegInit(0.U(3.W))
    //输出任务头部信息数据
    val NetworkInterfaceDataOUTReg=RegInit(0.U.asTypeOf(new PcbReg))

    //目的地址地址保存寄存器
    val destReg = RegInit(0.U(128.W))

    //userId寄存器
    val userIDReg = RegInit(0.U(4.W))

 
    val runCount = RegInit(0.U(3.W))

    io.runCount:=runCount   

    when(io.NetworkInterfaceDataIN.fire){
         when(userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).valid===false.B){
            //保存任务pcb表
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).ID:=io.NetworkInterfaceDataIN.bits.header.ID
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).MemInfo:=io.NetworkInterfaceDataIN.bits.header.ID
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).tagpacklength:=io.NetworkInterfaceDataIN.bits.header.tagpackalength
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).priority:=io.NetworkInterfaceDataIN.bits.header.priority
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).currpacklength:=1.U
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).finishpacklength:=0.U
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).config:=io.NetworkInterfaceDataIN.bits.load
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).valid:=true.B
            userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).runned:=false.B //还未执行过

            //将数据传输到任务分配的存储区域
            usermemeoryqueues_ports(io.NetworkInterfaceDataIN.bits.header.ID).enq.valid:=io.NetworkInterfaceDataIN.valid  
            io.NetworkInterfaceDataIN.bits<>usermemeoryqueues_ports(io.NetworkInterfaceDataIN.bits.header.ID).enq.bits
         }.elsewhen(userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).valid===true.B){
            when(io.NetworkInterfaceDataIN.bits.header.flit_type === FlitTypes.body|| io.NetworkInterfaceDataIN.bits.header.flit_type === FlitTypes.tail){
                when(userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).currpacklength ===datalen.U){
                    runCount:=runCount+1.U
                    InterfaceuseroutvldReg:=true.B
                    //存入cc的数据
                    NetworkInterfaceDataOUTReg.ID:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).ID
                    NetworkInterfaceDataOUTReg.MemInfo:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).MemInfo
                    NetworkInterfaceDataOUTReg.tagpacklength:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).tagpacklength
                    NetworkInterfaceDataOUTReg.currpacklength:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).currpacklength+1.U
                    NetworkInterfaceDataOUTReg.priority:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).priority
                    NetworkInterfaceDataOUTReg.valid:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).valid
                    NetworkInterfaceDataOUTReg.finishpacklength:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).finishpacklength
                    NetworkInterfaceDataOUTReg.runned:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).runned
                }
                userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).currpacklength:=userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).currpacklength+1.U
                    //将数据传输到任务分配的存储区域
                usermemeoryqueues_ports(io.NetworkInterfaceDataIN.bits.header.ID).enq.valid:=io.NetworkInterfaceDataIN.valid  
                io.NetworkInterfaceDataIN.bits<>usermemeoryqueues_ports(io.NetworkInterfaceDataIN.bits.header.ID).enq.bits
            }
         }


    }

    val potmul_timgReg = RegInit(10.U(64.W))

    val randomReg1 = RegInit(0.U(128.W))
    val randomReg2 = RegInit(0.U(128.W))
    //Interface状态机
    val idle  :: configData::waitresult ::transmitresut::transmitresut1::transmitresut2::transmitresut3::done ::Nil = Enum(8)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when(io.NetworkInterfaceuserIn.fire){
                potmul_timgReg:=10.U
                runnedReg:=true.B //表示当前有任务正在进行
                userIDReg:=io.NetworkInterfaceuserIn.bits  //当前任务处理的ID
                //将此用户fifo打开
                stateReg:=configData
            }

        }
        is(configData){
            DataInReg(0).header.flit_type:=FlitTypes.head
            DataInReg(0).header.ID:=userIDReg
            DataInReg(0).header.currpackagelength:=3.U   //result_len+1
            DataInReg(0).header.tagpackalength:=userInfomationRegs(userIDReg).tagpacklength
            DataInReg(0).header.priority:=userInfomationRegs(userIDReg).priority
            DataInReg(0).load:= userInfomationRegs(userIDReg).config      //配置空包

            DataInReg(1).header.flit_type:=FlitTypes.body   //将结果缓存下来
            DataInReg(1).header.ID:=userIDReg
            DataInReg(1).header.currpackagelength:=3.U   //result_len+1
            DataInReg(1).header.tagpackalength:=userInfomationRegs(userIDReg).tagpacklength
            DataInReg(1).header.priority:=userInfomationRegs(userIDReg).priority
            DataInReg(1).load:=BigInt(128, Random).U

            DataInReg(2).header.flit_type:=FlitTypes.tail   //将结果缓存下来
            DataInReg(2).header.ID:=userIDReg
            DataInReg(2).header.currpackagelength:=3.U   //result_len+1
            DataInReg(2).header.tagpackalength:=userInfomationRegs(userIDReg).tagpacklength
            DataInReg(2).header.priority:=userInfomationRegs(userIDReg).priority
            DataInReg(2).load:=BigInt(128, Random).U
            stateReg:=waitresult
        }
        is(waitresult){
            potmul_timgReg:=potmul_timgReg-1.U
            when(potmul_timgReg === 0.U){
                InterfaceDataOutvldReg:=true.B
                stateReg:=transmitresut1
            }
        }
        is(transmitresut1){
            when(io.NetworkInterfaceDataOUT.fire){
                InterfaceDataOutvldReg:=false.B
                resultlentReg:=1.U 
                stateReg:=transmitresut2
            }
        }
        is(transmitresut2){
            InterfaceDataOutvldReg:=true.B
            when(io.NetworkInterfaceDataOUT.fire){
                InterfaceDataOutvldReg:=false.B
                resultlentReg:=2.U 
                stateReg:=transmitresut3
            }
        }
        is(transmitresut3){
            InterfaceDataOutvldReg:=true.B
            when(io.NetworkInterfaceDataOUT.fire){
                InterfaceDataOutvldReg:=false.B


                stateReg:=done
            }
        }
        is(done){
                runCount:=runCount-1.U  

                userInfomationRegs(userIDReg).valid:=false.B
                configReg.cfg_len:=cfglen.asUInt
                configReg.data_len:=datalen.asUInt
                configReg.key_len:=keylen.asUInt
                configReg.result_len:=resultlen.asUInt
                potmul_timgReg:=10.U
                InterfaceuserinrdyReg:=true.B 
                runnedReg:=false.B
                resultlentReg:=0.U
                for(i<-0 until 3){
                    DataInReg(i):=0.U.asTypeOf(new Flit)
                }          
                stateReg:=idle          
        }      
    }

    when(io.NetworkInterfaceuserOUT.fire){
        InterfaceuseroutvldReg:=false.B
    }

    when(io.NetworkInterfaceuserIn.fire){
        InterfaceuserinrdyReg:=false.B
    }

    when(runCount === 4.U){
        InterfaceDataInrdyReg:=false.B
    }

    when(runCount < 4.U){
        InterfaceDataInrdyReg:=true.B
    }
    io.NetworkInterfaceuserOUT.bits<>NetworkInterfaceDataOUTReg


    DataInReg(resultlentReg)<>io.NetworkInterfaceDataOUT.bits

    io.runned:=runnedReg
} 

class Top_addsupnetworkadaptor(inPorts:Int) extends  Module {
    val io = IO(new NetworkInterfaceArbiterBundle_Top(inPorts))
    val trans_addsup= Module(new addsuptest(1,0,4,2))
    val test_module = Module(new networkModule1(inPorts))

    val top_queue = Module(new Queue(new Flit,16))
    val Topqueues = Seq.fill(inPorts)(Module(new Queue(new Flit, 16)))
    val Topqueues_ports = Wire(Vec(inPorts, chiselTypeOf(Topqueues(0).io)))
    Topqueues_ports.zip(Topqueues).foreach{case(m, n) =>
        m<>n.io
    }
    for(i<-0 until inPorts){
        Topqueues_ports(i).enq<>io.inputs(i)
        test_module.io.inputs(i)<>Topqueues_ports(i).deq
    }
    test_module.io.output<>trans_addsup.io.NetworkInterfaceDataIN
    top_queue.io.enq<>trans_addsup.io.NetworkInterfaceDataOUT  
    io.output<>top_queue.io.deq 

    io.runCount:=trans_addsup.io.runCount
}

//cfg:: 1，0，2,2
class addsuptest(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends Module{
    val io = IO(new Bundle {
    val NetworkInterfaceDataIN = Flipped(Decoupled(new Flit))
    val NetworkInterfaceDataOUT = Decoupled(new Flit)


    val runCount = Output(UInt(3.W))
    })
    val CC = Module(new Centralcontrol_unfiy)

    val transmitdataaddsup = Module(new test_transmitDatapotaddsup(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int))
    val queue_in = Module(new Queue(new Flit,16))
    val queue_out = Module(new Queue(new Flit,16))


    io.NetworkInterfaceDataIN<>queue_in.io.enq 
    queue_in.io.deq<>transmitdataaddsup.io.NetworkInterfaceDataIN

    queue_out.io.enq<>transmitdataaddsup.io.NetworkInterfaceDataOUT
    io.NetworkInterfaceDataOUT<>queue_out.io.deq 

    transmitdataaddsup.io.NetworkInterfaceuserOUT<>CC.io.NetworkInterfaceuserIn
    CC.io.NetworkInterfaceuserOUT<>transmitdataaddsup.io.NetworkInterfaceuserIn 
    CC.io.runned:=transmitdataaddsup.io.runned
    io.runCount:=transmitdataaddsup.io.runCount
}
