package NetworkInterface
import mesh_network._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import os.truncate
import NetworkInterfaceArbiter._
import clusterSM4._
 import java.io.DataInput
import  Interline._
class test_transmitDataSM3(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends  Module {
    val io = IO(new test_InterfaceSM3CCBundle)
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

    val DataOutVldReg = RegInit(false.B)  
    val DataInRdyReg=RegInit(false.B)
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
    val userIDReg = RegInit(0.U(3.W))

    val runCount = RegInit(0.U(3.W))

    val Tcp_Count = RegInit(0.U(3.W))

    io.runCount:=runCount
    when(io.NetworkInterfaceDataIN.fire){
         when(userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).valid===false.B){
            Tcp_Count:=Tcp_Count+1.U
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


    //Interface状态机
    val idle ::loadcfg :: loaddata  :: waitresult ::transmitresut::transmitresut1::transmitresut2::done ::Nil = Enum(8)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when(io.NetworkInterfaceuserIn.fire){
                runnedReg:=true.B //表示当前有任务正在进行
                userIDReg:=io.NetworkInterfaceuserIn.bits  //当前任务处理的ID

                //将此用户fifo打开
                userportsrdys(io.NetworkInterfaceuserIn.bits):=true.B
                stateReg:=loadcfg
                
            }
        }
        is(loadcfg){
            when(configReg.cfg_len =/= 0.U){
                when(usermemeoryqueues_ports(userIDReg).deq.fire){
                    DataOutReg.Address:=cfglen.asUInt-configReg.cfg_len
                    DataOutReg.flag:=0.U
                    DataOutReg.data:=datalen.U

                    DataOutVldReg:=true.B 
                }
                when(io.DataOut.fire){
                    DataOutVldReg:=false.B
                    configReg.cfg_len:=configReg.cfg_len-1.U
                }
            }.otherwise{
                    when(configReg.data_len =/= 0.U){
                        userportsrdys(userIDReg):=true.B
                        stateReg:=loaddata
                    }.otherwise{
                        stateReg:=idle
                    }

            }             
        }

        is(loaddata){
            when(configReg.data_len =/=0.U){
                when(usermemeoryqueues_ports(userIDReg).deq.fire){

                    DataOutReg.Address:=datalen.asUInt-configReg.data_len
                    DataOutReg.flag:=2.U
                    DataOutReg.data:=usermemeoryqueues_ports(userIDReg).deq.bits.load.asTypeOf(new DataFlitLoad).data  

                    DataOutVldReg:=true.B                    
                }
                when(io.DataOut.fire){
                    userportsrdys(userIDReg):=true.B                       
                    DataOutVldReg:=false.B    
                    configReg.data_len:=configReg.data_len - 1.U  
                }
            }
            when(configReg.data_len === 0.U){
                DataInReg(0).header.flit_type:=FlitTypes.head
                DataInReg(0).header.ID:=userIDReg
                DataInReg(0).header.currpackagelength:=2.U   //result_len+1
                DataInReg(0).header.tagpackalength:=userInfomationRegs(userIDReg).tagpacklength
                DataInReg(0).header.priority:=userInfomationRegs(userIDReg).priority
                DataInReg(0).load:= userInfomationRegs(userIDReg).config                       //配置空包

                DataInRdyReg:=true.B
                stateReg:=waitresult
            }
        }
        is(waitresult){
            when(configReg.result_len =/= 0.U){
                when(io.DataIn.fire){
                    when(configReg.result_len === 2.U){
                        DataInReg(1).header.flit_type:=FlitTypes.body   //将结果缓存下来
                        DataInReg(1).header.ID:=userIDReg
                        DataInReg(1).header.currpackagelength:=2.U   //result_len+1
                        DataInReg(1).header.tagpackalength:=userInfomationRegs(userIDReg).tagpacklength
                        DataInReg(1).header.priority:=userInfomationRegs(userIDReg).priority
                        DataInReg(1).load:=io.DataIn.bits.data
                    }.elsewhen(configReg.result_len === 1.U){
                        DataInReg(2).header.flit_type:=FlitTypes.tail   //将结果缓存下来
                        DataInReg(2).header.ID:=userIDReg
                        DataInReg(2).header.currpackagelength:=2.U   //result_len+1
                        DataInReg(2).header.tagpackalength:=userInfomationRegs(userIDReg).tagpacklength
                        DataInReg(2).header.priority:=userInfomationRegs(userIDReg).priority
                        DataInReg(2).load:=io.DataIn.bits.data                        
                    }
                    DataInRdyReg:=false.B
                    InterfaceDataOutvldReg:=true.B

                }
                when(io.NetworkInterfaceDataOUT.fire){
                    InterfaceDataOutvldReg:=false.B
                    DataInRdyReg:=true.B
                    resultlentReg:=resultlentReg+1.U;       
                    configReg.result_len:=configReg.result_len-1.U          
                }
            }.otherwise{
                 DataInRdyReg:=false.B  
                 InterfaceDataOutvldReg:=true.B 
                when(io.NetworkInterfaceDataOUT.fire){
                    InterfaceDataOutvldReg:=false.B 
                    stateReg:=done          
                }

            }
        }
        is(done){
                runCount:=runCount-1.U  
                Tcp_Count:=Tcp_Count-1.U

                userInfomationRegs(userIDReg).valid:=false.B
                configReg.cfg_len:=cfglen.asUInt
                configReg.data_len:=datalen.asUInt
                configReg.key_len:=keylen.asUInt
                configReg.result_len:=resultlen.asUInt
                InterfaceuserinrdyReg:=true.B 
                runnedReg:=false.B
                resultlentReg:=0.U
                stateReg:=idle          
        }      
    }

    when(io.NetworkInterfaceuserOUT.fire){
        InterfaceuseroutvldReg:=false.B
    }

    when(io.NetworkInterfaceuserIn.fire){
        InterfaceuserinrdyReg:=false.B
    }
    io.NetworkInterfaceuserOUT.bits<>NetworkInterfaceDataOUTReg

    io.DataOut.bits.data:=DataOutReg.data
    io.DataOut.bits.field.Address:=DataOutReg.Address
    io.DataOut.bits.field.flag:=DataOutReg.flag

    DataInReg(resultlentReg)<>io.NetworkInterfaceDataOUT.bits


    io.DataOut.valid:=DataOutVldReg
    io.DataIn.ready:=DataInRdyReg
    io.runned:=runnedReg


    io.tcp_Count:= Tcp_Count //实际有多少Tcp

    when(runCount === 4.U){
        InterfaceDataInrdyReg:=false.B
    }

    when(runCount < 4.U){
        InterfaceDataInrdyReg:=true.B
    }

} 


//调度模块
class Centralcontrol_unfiy extends Module{
    val io = IO(new CCBundle)

    val InterfaceuseInrdyReg = RegInit(true.B)
    val InterfaceuseoutvldReg = RegInit(false.B)

    io.NetworkInterfaceuserIn.ready:=InterfaceuseInrdyReg

    io.NetworkInterfaceuserOUT.valid:=InterfaceuseoutvldReg
        // 优先级fifo   优先级分为4级： 0、1、2、3
    val userLinkFifo = Seq.fill(4)(Module(new Queue(UInt(4.W), 8)))
    val userLinkFifo_ports = Wire(Vec(4, chiselTypeOf(userLinkFifo(0).io)))

    val userLinkFifosenqvldReg =RegInit(VecInit.fill(4)(false.B))
    val userLinkFifosdeqrdyReg =RegInit(VecInit.fill(4)(false.B))

    userLinkFifo_ports.zip(userLinkFifo).foreach{case(m, n) =>
        m<>n.io
    }
    val tmpuserIdReg = RegInit(0.U(3.W))

    for(i<-0 until 4){
        userLinkFifo_ports(i).enq.valid:=userLinkFifosenqvldReg(i)
        userLinkFifo_ports(i).enq.bits:=DontCare
        userLinkFifo_ports(i).deq.ready:=userLinkFifosdeqrdyReg(i)

        when(userLinkFifo_ports(i).enq.fire){
            userLinkFifosenqvldReg(i):=false.B
        }
        when(userLinkFifo_ports(i).deq.fire){
            userLinkFifosdeqrdyReg(i):=false.B
        }
    }

    //进入就绪队列中
    when(io.NetworkInterfaceuserIn.fire){
        userLinkFifosenqvldReg(io.NetworkInterfaceuserIn.bits.priority):=true.B
        userLinkFifo_ports(io.NetworkInterfaceuserIn.bits.priority).enq.bits:=io.NetworkInterfaceuserIn.bits.ID
    }

    val idle::late::loaduserID::done::Nil = Enum(4)
    val stateReg=RegInit(idle)

    switch(stateReg ){
        is(idle){
            when(io.runned === false.B){
                when(userLinkFifo_ports(3).deq.valid){
                    userLinkFifosdeqrdyReg(3):=true.B       
                    tmpuserIdReg:=userLinkFifo_ports(3).deq.bits
                    stateReg:=late
                }.elsewhen(userLinkFifo_ports(2).deq.valid){
                    userLinkFifosdeqrdyReg(2):=true.B 
                    tmpuserIdReg:=userLinkFifo_ports(2).deq.bits
                    stateReg:=late 
                }.elsewhen(userLinkFifo_ports(1).deq.valid){
                    
                    userLinkFifosdeqrdyReg(1):=true.B 
                    tmpuserIdReg:=userLinkFifo_ports(1).deq.bits
                    stateReg:=late 
                }.elsewhen(userLinkFifo_ports(0).deq.valid){

                    userLinkFifosdeqrdyReg(0):=true.B 
                    tmpuserIdReg:=userLinkFifo_ports(0).deq.bits
                    stateReg:=late 
                }.otherwise{
                    stateReg:=idle
                }
            }.elsewhen(io.runned === true.B){
                stateReg:=idle
            }                
        }
        is(late){
            InterfaceuseoutvldReg:=true.B
            stateReg:=loaduserID
        }
        is(loaduserID){
            when(io.NetworkInterfaceuserOUT.fire){
                InterfaceuseoutvldReg:=false.B
                stateReg:=done
            }
        }
        is(done){
            when(io.runned === false.B){
                tmpuserIdReg:=0.U
                stateReg:=idle
            }              
        }
    }

    io.NetworkInterfaceuserOUT.bits:=tmpuserIdReg
}


class Top_sm3CCnetworkadaptor(inPorts:Int) extends  Module {
    val io = IO(new sm3NetworkInterfaceArbiterBundle_Top(inPorts))
    val trans_sm3128 = Module(new SM3test(1,0,1,2))
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
    test_module.io.output<>trans_sm3128.io.NetworkInterfaceDataIN
    top_queue.io.enq<>trans_sm3128.io.NetworkInterfaceDataOUT  
    io.output<>top_queue.io.deq 

        io.runCount:=trans_sm3128.io.runCOunt
        io.tcpCount:=trans_sm3128.io.tcpCount
}

class Top_sm3CCnetworkadaptor512(inPorts:Int) extends  Module {
    val io = IO(new sm3NetworkInterfaceArbiterBundle_Top(inPorts))
    val trans_SM3512 = Module(new SM3test(1,0,4,2))
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
    test_module.io.output<>trans_SM3512.io.NetworkInterfaceDataIN
    top_queue.io.enq<>trans_SM3512.io.NetworkInterfaceDataOUT  
    io.output<>top_queue.io.deq 

    io.runCount:=trans_SM3512.io.runCOunt
    io.tcpCount:=trans_SM3512.io.tcpCount
}

//cfg:: 1，0，2,2
class SM3test(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends Module{
    val io = IO(new Bundle {
    val NetworkInterfaceDataIN = Flipped(Decoupled(new Flit))
    val NetworkInterfaceDataOUT = Decoupled(new Flit)

    val runCOunt = Output(UInt(3.W))
    val tcpCount = Output(UInt(3.W))
    })
    val CC = Module(new Centralcontrol_unfiy)

    val sm3 = Module(new unique_SM3Module)
    val transmitdata = Module(new test_transmitDataSM3(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int))
    val queue_in = Module(new Queue(new Flit,16))
    val queue_out = Module(new Queue(new Flit,16))


    io.NetworkInterfaceDataIN<>queue_in.io.enq 
    queue_in.io.deq<>transmitdata.io.NetworkInterfaceDataIN

    queue_out.io.enq<>transmitdata.io.NetworkInterfaceDataOUT
    io.NetworkInterfaceDataOUT<>queue_out.io.deq 

    transmitdata.io.NetworkInterfaceuserOUT<>CC.io.NetworkInterfaceuserIn
    CC.io.NetworkInterfaceuserOUT<>transmitdata.io.NetworkInterfaceuserIn 
    CC.io.runned:=transmitdata.io.runned

    transmitdata.io.DataOut<>sm3.io.DataIn
    transmitdata.io.DataIn<>sm3.io.DataOut

    io.runCOunt:=transmitdata.io.runCount
    io.tcpCount:=transmitdata.io.tcp_Count
}






