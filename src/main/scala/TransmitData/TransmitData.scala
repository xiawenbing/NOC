package NetworkInterface
import mesh_network._
import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import os.truncate
import NetworkInterfaceArbiter._
import clusterSM4._
 import java.io.DataInput



 class test_transmitData(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends  Module {
    val io = IO(new test_InterfaceBundle)
     /*
    InterfaceDataInrdyReg:寄存器与输入io的ready信号连接
    InterfaceDataOutvldReg:寄存器与输入io的ready信号连接
    DataInRdyReg : 寄存器与输入io的ready信号连接
    DataOutVldReg ： 寄存器与输出io的valid 信号连接
    */ 
    val InterfaceDataInrdyReg = RegInit(true.B)
    val InterfaceDataOutvldReg=RegInit(false.B)
    val DataOutVldReg = RegInit(false.B)  
    val DataInRdyReg=RegInit(false.B)

    io.NetworkInterfaceDataIN.ready:=InterfaceDataInrdyReg
    io.NetworkInterfaceDataOUT.valid:=InterfaceDataOutvldReg
    io.DataOut.valid:=DataOutVldReg
    io.DataIn.ready:=DataInRdyReg


    //缓冲配置数据
    val configReg = RegInit((new configlenRegBundle).Lit(_.data_len -> datalen.U,
                                                         _.key_len -> keylen.U,
                                                         _.cfg_len ->cfglen.U,
                                                         _.result_len->resultlen.U))


    //输出数据寄存器
    val DataOutReg = RegInit(0.U.asTypeOf(new test_UnifyfieldReg))

    //输出结果寄存器
    val DataInReg = RegInit(0.U.asTypeOf(new Flit))

    //Interface状态机
    val idle :: loadkey :: loaddata  :: waitresult :: done ::Nil = Enum(5)
    val stateReg = RegInit(idle)


    def OpenoutputdataChanel ={
            InterfaceDataInrdyReg:=false.B 
            DataOutVldReg:=true.B            
    }
 
    def CloseoutputdataChanel ={
            InterfaceDataInrdyReg:=true.B 
            DataOutVldReg:=false.B            
    }
    switch(stateReg){
        is(idle){
            when(configReg.cfg_len =/= 0.U){
                when(io.NetworkInterfaceDataIN.fire){
                    DataOutReg.Address:=cfglen.asUInt-configReg.cfg_len
                    DataOutReg.flag:=0.U
                    DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data 
                    OpenoutputdataChanel
                }
                when(io.DataOut.fire){
                    configReg.cfg_len:=configReg.cfg_len-1.U
                }
            }.otherwise{
                when(configReg.key_len =/=0.U){
                    CloseoutputdataChanel
                    stateReg:=loadkey
                }.otherwise{
                    when(configReg.data_len =/= 0.U){
                        CloseoutputdataChanel
                        stateReg:=loaddata
                    }.otherwise{
                        stateReg:=idle
                    }
                }
            }
        }
        is(loadkey){
            when(configReg.key_len =/= 0.U){
                when(io.NetworkInterfaceDataIN.fire){
                    DataOutReg.Address:=keylen.asUInt-configReg.key_len
                    DataOutReg.flag:=1.U
                    DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data     
                    OpenoutputdataChanel                 
                }
                when(io.DataOut.fire){
                    configReg.key_len:=configReg.key_len-1.U                
                }
            }
            when(configReg.key_len === 0.U){
                CloseoutputdataChanel
                when(configReg.data_len =/=0.U){
                    stateReg:=loaddata
                }
            }
        }
        is(loaddata){
            when(configReg.data_len =/=0.U){
                when(io.NetworkInterfaceDataIN.fire){
                    DataOutReg.Address:=datalen.asUInt-configReg.data_len
                    DataOutReg.flag:=2.U
                    DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data       
                    OpenoutputdataChanel                 
                }
                when(io.DataOut.fire){
                    when(configReg.data_len === 1.U){
                        DataOutVldReg:=false.B    
                        configReg.data_len:=configReg.data_len - 1.U  
                    }.otherwise{
                        CloseoutputdataChanel
                        configReg.data_len:=configReg.data_len - 1.U  
                    }          
                }
            }
            when(configReg.data_len === 0.U){
                DataInRdyReg:=true.B
                InterfaceDataInrdyReg:=false.B
                stateReg:=waitresult
            }
        }
        is(waitresult){
            when(configReg.result_len =/= 0.U){
                when(io.DataIn.fire){
                    DataInReg.header.flit_type:=FlitTypes.body 
                    DataInReg.load:=io.DataIn.bits.data
                    DataInRdyReg:=false.B
                    InterfaceDataOutvldReg:=true.B 
                }
                when(io.NetworkInterfaceDataOUT.fire){
                    InterfaceDataOutvldReg:=false.B  
                    DataInRdyReg:=true.B       
                    configReg.result_len:=configReg.result_len-1.U            
                }
            }.otherwise{
                 stateReg:=done
            }
        }
        is(done){
                InterfaceDataOutvldReg:=false.B  
                DataInRdyReg:=false.B 
                CloseoutputdataChanel    
                configReg.cfg_len:=cfglen.asUInt
                configReg.data_len:=datalen.asUInt
                configReg.key_len:=keylen.asUInt
                configReg.result_len:=resultlen.asUInt   
                stateReg:=idle                  
        }
    }
    io.DataOut.bits.data:=DataOutReg.data
    io.DataOut.bits.field.Address:=DataOutReg.Address
    io.DataOut.bits.field.flag:=DataOutReg.flag
    io.NetworkInterfaceDataOUT.bits:=DataInReg
} 
 /***************************************************adapter1.1***********************************************************/

class Toptest(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends Module{
    val io = IO(new Bundle {
    val NetworkInterfaceDataIN = Flipped(Decoupled(new Flit))
    val NetworkInterfaceDataOUT = Decoupled(new Flit)
    })
    
    val aes = Module(new uintSM4)
    val transmitdata = Module(new test_transmitData1(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int))
    val queue_in = Module(new Queue(new Flit,16))
    val queue_out = Module(new Queue(new Flit,16))

    queue_in.io.enq<>io.NetworkInterfaceDataIN
    transmitdata.io.NetworkInterfaceDataIN<>queue_in.io.deq

    queue_out.io.enq<>transmitdata.io.NetworkInterfaceDataOUT
    io.NetworkInterfaceDataOUT<>queue_out.io.deq   

    transmitdata.io.DataOut<>aes.io.DataIn
    transmitdata.io.DataIn<>aes.io.DataOut
}



class Top_networkadaptor(inPorts:Int) extends  Module {
    val io = IO(new NetworkInterfaceArbiterBundle_Top(inPorts))
    val trans_aes = Module(new Toptest(1,1,1,1))
    val test_module = Module(new networkModule0(inPorts))
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
    test_module.io.output<>trans_aes.io.NetworkInterfaceDataIN
    top_queue.io.enq<>trans_aes.io.NetworkInterfaceDataOUT  
    io.output<>top_queue.io.deq 
}

class Top_SM4networkadaptor(inPorts:Int) extends  Module {
    val io = IO(new NetworkInterfaceArbiterBundle_Top(inPorts))
    val trans_aes = Module(new Toptest(1,1,1,1))
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
    test_module.io.output<>trans_aes.io.NetworkInterfaceDataIN
    top_queue.io.enq<>trans_aes.io.NetworkInterfaceDataOUT  
    io.output<>top_queue.io.deq 
}


class test_transmitData1(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends  Module {
    val io = IO(new test_InterfaceBundle)
     /*
    InterfaceDataInrdyReg:寄存器与输入io的ready信号连接
    InterfaceDataOutvldReg:寄存器与输入io的ready信号连接
    DataInRdyReg : 寄存器与输入io的ready信号连接
    DataOutVldReg ： 寄存器与输出io的valid 信号连接
    */ 
    val InterfaceDataInrdyReg = RegInit(true.B)
    val InterfaceDataOutvldReg=RegInit(false.B)
    val DataOutVldReg = RegInit(false.B)  
    val DataInRdyReg=RegInit(false.B)

    io.NetworkInterfaceDataIN.ready:=InterfaceDataInrdyReg
    io.NetworkInterfaceDataOUT.valid:=InterfaceDataOutvldReg
    io.DataOut.valid:=DataOutVldReg
    io.DataIn.ready:=DataInRdyReg

    val userLinkFifo=Module(new Queue(UInt(4.W), 8))
    val userLinkFifovldReg = RegInit(false.B)
    val userLinkFifordyReg = RegInit(false.B)

    userLinkFifo.io.enq.valid:=userLinkFifovldReg //控制用户链表输入valid
    userLinkFifo.io.deq.ready:=userLinkFifordyReg //控制用户链表输出ready
    userLinkFifo.io.enq.bits:=DontCare
    val userLinkReg = RegInit(0.U(4.W))
    //PCB表
    val userInfomationRegs =RegInit(VecInit.fill(8)(0.U.asTypeOf(new PcbReg)))
    val curruserPCB = RegInit(0.U.asTypeOf(new PcbReg))
    val tmpuserID =RegInit(0.U(4.W))
    //用户数据存储器
    val userqueues = Seq.fill(8)(Module(new Queue(new Flit, 16)))
    val userqueues_ports = Wire(Vec(8, chiselTypeOf(userqueues(0).io)))
    userqueues_ports.zip(userqueues).foreach{case(m, n) =>
        m<>n.io
    }
    val userportsrdys=RegInit(VecInit.fill(8)(false.B))
    for(i<-0 until 8){
        userqueues_ports(i).enq.valid:=false.B
        userqueues_ports(i).enq.bits:=DontCare
        userqueues_ports(i).deq.ready:=userportsrdys(i)
    }

    //缓冲配置数据
    val configReg = RegInit((new configlenRegBundle).Lit(_.data_len -> datalen.U,
                                                         _.key_len -> keylen.U,
                                                         _.cfg_len ->cfglen.U,
                                                         _.result_len->resultlen.U))

    
    //输出数据寄存器
    val DataOutReg = RegInit(0.U.asTypeOf(new test_UnifyfieldReg))

    //输出结果寄存器
    val DataInReg = RegInit(0.U.asTypeOf(new Flit))

    //Interface状态机
    val idle :: loadkey :: loaddata  :: waitresult :: done ::recover::checkuserMemy::rloadcfg::rloadkey::rloaddata::Nil = Enum(10)
    val stateReg = RegInit(idle)


    def OpenoutputdataChanel ={
            InterfaceDataInrdyReg:=false.B 
            DataOutVldReg:=true.B            
    }
 
    def CloseoutputdataChanel ={
            InterfaceDataInrdyReg:=true.B 
            DataOutVldReg:=false.B            
    }

    userLinkFifo.io.enq.bits:=userLinkReg
    // def runnedUintcfg = {
        
    // }
    switch(stateReg){
        is(idle){
            userLinkFifovldReg:=false.B
            when(configReg.cfg_len =/= 0.U){
                when(io.NetworkInterfaceDataIN.fire){
                    when(curruserPCB.valid === false.B){
                        //-----------------------------------------创建PCB表----------------------------------------//
                        curruserPCB.ID:=io.NetworkInterfaceDataIN.bits.header.ID
                        curruserPCB.MemInfo:=io.NetworkInterfaceDataIN.bits.header.ID
                        curruserPCB.tagpacklength:=io.NetworkInterfaceDataIN.bits.header.tagpackalength
                        curruserPCB.priority:=io.NetworkInterfaceDataIN.bits.header.priority
                        curruserPCB.currpacklength:=0.U
                        curruserPCB.runned:=true.B  //已经执行过一次
                        curruserPCB.valid:=true.B
                        //-----------------------------------------------------------------------------------------//
                        DataOutReg.Address:=cfglen.asUInt-configReg.cfg_len
                        DataOutReg.flag:=0.U
                        DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data 
                        OpenoutputdataChanel
                    }.otherwise{
                        when(io.NetworkInterfaceDataIN.bits.header.ID === curruserPCB.ID){
                            configReg.cfg_len:=configReg.cfg_len
                            configReg.data_len:=datalen.asUInt
                            configReg.key_len:=0.U
                            configReg.result_len:=resultlen.asUInt 

                            DataOutReg.Address:=cfglen.asUInt-configReg.cfg_len
                            DataOutReg.flag:=0.U
                            DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data   //作为后续数据流切换入口
                            OpenoutputdataChanel                                                      
                        }.otherwise{
                            when(io.NetworkInterfaceDataIN.bits.header.priority > curruserPCB.priority){
                                userLinkFifovldReg:=true.B
                                userLinkReg:=curruserPCB.ID

                                curruserPCB.ID:=io.NetworkInterfaceDataIN.bits.header.ID
                                curruserPCB.MemInfo:=io.NetworkInterfaceDataIN.bits.header.ID
                                curruserPCB.tagpacklength:=io.NetworkInterfaceDataIN.bits.header.tagpackalength
                                curruserPCB.priority:=io.NetworkInterfaceDataIN.bits.header.priority
                                curruserPCB.currpacklength:=0.U
                                curruserPCB.valid:=true.B
                                curruserPCB.runned:=true.B  //已经执行过一次

                                DataOutReg.Address:=cfglen.asUInt-configReg.cfg_len
                                DataOutReg.flag:=0.U
                                DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data 

                                configReg.data_len:=datalen.asUInt
                                configReg.key_len:=keylen.asUInt
                                configReg.result_len:=resultlen.asUInt 
                                OpenoutputdataChanel

                                 //输入切换的队列  
                                userInfomationRegs(curruserPCB.ID)<>curruserPCB //保存PCB                                       

                             }.otherwise{
                                when(userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).valid){
                                    userqueues_ports(io.NetworkInterfaceDataIN.bits.header.ID).enq.valid:=io.NetworkInterfaceDataIN.valid
                                    io.NetworkInterfaceDataIN.bits<>userqueues_ports(io.NetworkInterfaceDataIN.bits.header.ID).enq.bits
                                    //将数据传入Memory
                                    
                                    stateReg:=idle
                                    
                                }.otherwise{
                                    userLinkFifovldReg:=true.B
                                    userLinkReg:=io.NetworkInterfaceDataIN.bits.header.ID  //保存优先级低的队列
                                    //保存PCB   
                                    userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).ID:=io.NetworkInterfaceDataIN.bits.header.ID
                                    userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).MemInfo:=io.NetworkInterfaceDataIN.bits.header.ID
                                    userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).tagpacklength:=io.NetworkInterfaceDataIN.bits.header.tagpackalength
                                    userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).priority:=io.NetworkInterfaceDataIN.bits.header.priority
                                    userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).currpacklength:=0.U
                                    userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).valid:=true.B
                                    userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).runned:=false.B //还未执行过
                                    //io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data 传输到fifo中   

                                    userqueues_ports(io.NetworkInterfaceDataIN.bits.header.ID).enq.valid:=io.NetworkInterfaceDataIN.valid    
                                    io.NetworkInterfaceDataIN.bits<>userqueues_ports(io.NetworkInterfaceDataIN.bits.header.ID).enq.bits      
                                    stateReg:=idle
                                }                               
                             }
                        }
                    }                 
                }
                when(io.DataOut.fire){
                    curruserPCB.currpacklength:=curruserPCB.currpacklength+1.U
                    userLinkFifovldReg:=false.B
                    configReg.cfg_len:=configReg.cfg_len-1.U
                }
            }.otherwise{
                userLinkFifovldReg:=false.B

                when(configReg.key_len =/=0.U){
                    CloseoutputdataChanel
                    stateReg:=loadkey
                }.otherwise{
                    when(configReg.data_len =/= 0.U){
                        CloseoutputdataChanel
                        stateReg:=loaddata
                    }.otherwise{
                        stateReg:=idle
                    }
                }
            }
        }
        is(loadkey){
            when(configReg.key_len =/= 0.U){
                when(io.NetworkInterfaceDataIN.fire){
                    curruserPCB.currpacklength:=curruserPCB.currpacklength+1.U

                    DataOutReg.Address:=keylen.asUInt-configReg.key_len
                    DataOutReg.flag:=1.U
                    DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data     
                    OpenoutputdataChanel                 
                }
                when(io.DataOut.fire){
                    configReg.key_len:=configReg.key_len-1.U                
                }
            }
            when(configReg.key_len === 0.U){
                CloseoutputdataChanel
                when(configReg.data_len =/=0.U){
                    stateReg:=loaddata
                }
            }
        }
        is(loaddata){
            when(configReg.data_len =/=0.U){
                when(io.NetworkInterfaceDataIN.fire){
                    //curruserPCB.currpacklength:=curruserPCB.currpacklength+1.U  

                    DataOutReg.Address:=datalen.asUInt-configReg.data_len
                    DataOutReg.flag:=2.U
                    DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data   
                    OpenoutputdataChanel                 
                }
                when(io.DataOut.fire){
                    when(configReg.data_len === 1.U){
                        DataOutVldReg:=false.B    
                        configReg.data_len:=configReg.data_len - 1.U  
                    }.otherwise{
                        CloseoutputdataChanel
                        configReg.data_len:=configReg.data_len - 1.U  
                    }          
                }
            }
            when(configReg.data_len === 0.U){
                curruserPCB.currpacklength:=curruserPCB.currpacklength+1.U
                DataInRdyReg:=true.B
                InterfaceDataInrdyReg:=false.B
                stateReg:=waitresult
            }
        }
        is(waitresult){
            when(configReg.result_len =/= 0.U){
                when(io.DataIn.fire){
                    DataInReg.header.flit_type:=FlitTypes.body 
                    DataInReg.load:=io.DataIn.bits.data
                    DataInRdyReg:=false.B
                    InterfaceDataOutvldReg:=true.B 
                }
                when(io.NetworkInterfaceDataOUT.fire){
                    InterfaceDataOutvldReg:=false.B  
                    DataInRdyReg:=true.B       
                    configReg.result_len:=configReg.result_len-1.U            
                }
            }.otherwise{
                 DataInRdyReg:=false.B  
                 stateReg:=done
            }
        }
        is(done){
                when(curruserPCB.currpacklength < curruserPCB.tagpacklength){
                    curruserPCB.runned:=true.B
                    userInfomationRegs(curruserPCB.ID).currpacklength:=curruserPCB.currpacklength
                    when(userqueues_ports(curruserPCB.ID).deq.valid){
                        stateReg:=checkuserMemy
                    }.otherwise{
                        CloseoutputdataChanel
                        configReg.cfg_len:=cfglen.asUInt
                        configReg.data_len:=datalen.asUInt
                        configReg.key_len:=0.U 
                        configReg.result_len:=resultlen.asUInt 
                        stateReg:=idle 
                    }
                }.elsewhen(curruserPCB.currpacklength === curruserPCB.tagpacklength){
                    userInfomationRegs(curruserPCB.ID).valid:=false.B
                    when(userLinkFifo.io.deq.valid){     // when(io.input.valid === false.B)  执行一次原子操作后恢复一次现场
                        //恢复现场
                        OpenoutputdataChanel

                        curruserPCB.valid:=false.B 
                        tmpuserID:=userLinkFifo.io.deq.bits
                        userLinkFifordyReg:=true.B
                        stateReg:=recover
                    }.otherwise{
                        CloseoutputdataChanel
                        curruserPCB.valid:=false.B
                        configReg.cfg_len:=cfglen.asUInt
                        configReg.data_len:=datalen.asUInt
                        configReg.key_len:=keylen.asUInt
                        configReg.result_len:=resultlen.asUInt 
                        stateReg:=idle 
                    }
                }                   
        }
        is(recover){
            when(userLinkFifo.io.deq.fire){
                userLinkFifordyReg:=false.B
                              
                curruserPCB.ID:= userInfomationRegs(tmpuserID).ID
                curruserPCB.MemInfo:=userInfomationRegs(tmpuserID).MemInfo
                curruserPCB.tagpacklength:=userInfomationRegs(tmpuserID).tagpacklength
                curruserPCB.priority:=userInfomationRegs(tmpuserID).priority
                curruserPCB.currpacklength:=userInfomationRegs(tmpuserID).currpacklength
                curruserPCB.runned:=userInfomationRegs(tmpuserID).runned
                curruserPCB.valid:=true.B  

                stateReg:=checkuserMemy                              
            }
        }
        is(checkuserMemy){
            when(userqueues_ports(curruserPCB.ID).deq.valid){
                when(curruserPCB.runned){
                    configReg.cfg_len:=cfglen.asUInt
                    configReg.data_len:=datalen.asUInt
                    configReg.key_len:=0.U  
                    //configReg.key_len:=keylen.U
                    configReg.result_len:=resultlen.asUInt

                    userportsrdys(curruserPCB.ID):=true.B

                    stateReg:=rloadcfg                        
                }.otherwise{
                    configReg.cfg_len:=cfglen.asUInt
                    configReg.data_len:=datalen.asUInt
                    configReg.key_len:=cfglen.U
                    configReg.result_len:=resultlen.asUInt

                    userportsrdys(curruserPCB.ID):=true.B
                    stateReg:=rloadcfg
                }
            }.otherwise{
                when(curruserPCB.runned){
                    CloseoutputdataChanel
                    configReg.cfg_len:=cfglen.asUInt
                    configReg.data_len:=datalen.asUInt
                    configReg.key_len:=0.U  
                    //configReg.key_len:=keylen.U
                    configReg.result_len:=resultlen.asUInt     
                    stateReg:=idle
                }.otherwise{
                    CloseoutputdataChanel
                    configReg.cfg_len:=cfglen.asUInt
                    configReg.data_len:=datalen.asUInt
                    
                    configReg.key_len:=keylen.U
                    configReg.result_len:=resultlen.asUInt     
                    stateReg:=idle                    
                }
                
            }
        }
        is(rloadcfg){
            when(configReg.cfg_len =/= 0.U){
                when(userqueues_ports(curruserPCB.ID).deq.fire){
                    DataOutReg.Address:=cfglen.asUInt-configReg.cfg_len
                    DataOutReg.flag:=0.U
                    // DataOutReg.data:=io.NetworkInterfaceDataIN.bits.load.asTypeOf(new DataFlitLoad).data 
                    DataOutReg.data:=userqueues_ports(curruserPCB.ID).deq.bits.load.asTypeOf(new DataFlitLoad).data

                    userportsrdys(curruserPCB.ID):=false.B
                    DataOutVldReg:=true.B 
                }
                when(io.DataOut.fire){
                    curruserPCB.currpacklength:=userInfomationRegs(tmpuserID).currpacklength+1.U
                    configReg.cfg_len:=configReg.cfg_len-1.U
                }
            }.otherwise{
                when(configReg.key_len =/=0.U){
                    userportsrdys(curruserPCB.ID):=true.B
                    DataOutVldReg:=false.B 
                    stateReg:=rloadkey
                }.otherwise{
                    when(configReg.data_len =/= 0.U){
                        userportsrdys(curruserPCB.ID):=true.B
                        DataOutVldReg:=false.B 
                        stateReg:=rloaddata
                    }.otherwise{
                        stateReg:=idle
                    }
                }
            }           
            
        }
        is(rloadkey){
            when(configReg.key_len =/= 0.U){
                when(userqueues_ports(curruserPCB.ID).deq.fire){
                    //curruserPCB.currpacklength:=curruserPCB.currpacklength+1.U

                    DataOutReg.Address:=keylen.asUInt-configReg.key_len
                    DataOutReg.flag:=1.U
                    DataOutReg.data:=userqueues_ports(curruserPCB.ID).deq.bits.load.asTypeOf(new DataFlitLoad).data 

                    userportsrdys(curruserPCB.ID):=false.B
                    DataOutVldReg:=true.B                 
                }
                when(io.DataOut.fire){
                    curruserPCB.currpacklength:= curruserPCB.currpacklength+1.U
                    configReg.key_len:=configReg.key_len-1.U                
                }
            }
            when(configReg.key_len === 0.U){
                userportsrdys(curruserPCB.ID):=true.B
                DataOutVldReg:=false.B  
                when(configReg.data_len =/=0.U){
                    stateReg:=rloaddata
                }
            }
        }
        is(rloaddata){
            when(configReg.data_len =/=0.U){
                when(userqueues_ports(curruserPCB.ID).deq.fire){

                    DataOutReg.Address:=datalen.asUInt-configReg.data_len
                    DataOutReg.flag:=2.U
                    DataOutReg.data:=userqueues_ports(curruserPCB.ID).deq.bits.load.asTypeOf(new DataFlitLoad).data  

                    userportsrdys(curruserPCB.ID):=false.B
                    DataOutVldReg:=true.B                    
                }
                when(io.DataOut.fire){
                    curruserPCB.currpacklength:= curruserPCB.currpacklength+1.U
                    when(configReg.data_len === 1.U){
                        DataOutVldReg:=false.B    
                        configReg.data_len:=configReg.data_len - 1.U  
                    }.otherwise{
                        userportsrdys(curruserPCB.ID):=true.B
                        DataOutVldReg:=false.B 
                        configReg.data_len:=configReg.data_len - 1.U  
                    }          
                }
            }
            when(configReg.data_len === 0.U){
                DataInRdyReg:=true.B
                userportsrdys(curruserPCB.ID):=false.B
                stateReg:=waitresult
            }
        }
    }

    io.DataOut.bits.data:=DataOutReg.data
    io.DataOut.bits.field.Address:=DataOutReg.Address
    io.DataOut.bits.field.flag:=DataOutReg.flag
    io.NetworkInterfaceDataOUT.bits:=DataInReg
} 

//调度模块
class Centralcontrol extends Module{
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
/**************************************运行队列****************************************************/


    val runLinkFifo = Seq.fill(4)(Module(new Queue(UInt(4.W), 8)))
    val runLinkFifo_ports = Wire(Vec(4, chiselTypeOf(userLinkFifo(0).io)))

    val runLinkFifosenqvldReg =RegInit(VecInit.fill(4)(false.B))
    val runLinkFifosdeqrdyReg =RegInit(VecInit.fill(4)(false.B))

    runLinkFifo_ports.zip(runLinkFifo).foreach{case(m, n) =>
        m<>n.io
    }

    for(i<-0 until 4){
        runLinkFifo_ports(i).enq.valid:=runLinkFifosenqvldReg(i)
        runLinkFifo_ports(i).enq.bits:=DontCare
        runLinkFifo_ports(i).deq.ready:=runLinkFifosdeqrdyReg(i)

        when(runLinkFifo_ports(i).enq.fire){
            runLinkFifosenqvldReg(i):=false.B
        }
        when(runLinkFifo_ports(i).deq.fire){
            runLinkFifosdeqrdyReg(i):=false.B
        }
    }

    val userInfomationRegs =RegInit(VecInit.fill(8)(0.U.asTypeOf(new PcbReg)))


    when(io.NetworkInterfaceuserIn.fire){
        when(userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).valid){
            userInfomationRegs((io.NetworkInterfaceuserIn.bits.ID)).currpacklength:=userInfomationRegs((io.NetworkInterfaceuserIn.bits.ID)).currpacklength+2.U
        }.elsewhen(userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).valid === false.B){
            //保存任务Tcb表
            userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).ID:=io.NetworkInterfaceuserIn.bits.ID
            userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).MemInfo:=io.NetworkInterfaceuserIn.bits.ID
            userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).tagpacklength:=io.NetworkInterfaceuserIn.bits.tagpacklength
            userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).priority:=io.NetworkInterfaceuserIn.bits.priority
            userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).currpacklength:=io.NetworkInterfaceuserIn.bits.currpacklength
            userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).valid:=true.B
            userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).runned:=false.B //还未执行过 
            userInfomationRegs(io.NetworkInterfaceuserIn.bits.ID).finishpacklength:=0.U
            //将user导入所属优先级队列中
            userLinkFifosenqvldReg(io.NetworkInterfaceuserIn.bits.priority):=true.B
            userLinkFifo_ports(io.NetworkInterfaceuserIn.bits.priority).enq.bits:=io.NetworkInterfaceuserIn.bits.ID
        }
    }
    


    val idle::runfifocheck::ischeckdata::loaduserID::reloaduserReg::loadlinkfifo::done::Nil = Enum(7)
    val stateReg=RegInit(idle)

    switch(stateReg){
        is(idle){
            when(io.runned === false.B){
                when(userLinkFifo_ports(3).deq.valid){
                    userLinkFifosdeqrdyReg(3):=true.B       
                    tmpuserIdReg:=userLinkFifo_ports(3).deq.bits
                    stateReg:=ischeckdata
                }.elsewhen(userLinkFifo_ports(2).deq.valid){
                    userLinkFifosdeqrdyReg(2):=true.B 
                    tmpuserIdReg:=userLinkFifo_ports(2).deq.bits
                    stateReg:=ischeckdata 
                }.elsewhen(userLinkFifo_ports(1).deq.valid){
                    userLinkFifosdeqrdyReg(1):=true.B 
                    tmpuserIdReg:=userLinkFifo_ports(1).deq.bits
                    stateReg:=ischeckdata 
                }.elsewhen(userLinkFifo_ports(0).deq.valid){
                    userLinkFifosdeqrdyReg(0):=true.B 
                    tmpuserIdReg:=userLinkFifo_ports(0).deq.bits
                    stateReg:=ischeckdata 
                }.otherwise{
                    stateReg:=runfifocheck
                }
            }.elsewhen(io.runned === true.B){
                stateReg:=idle
            }
        }
        is(runfifocheck){
                when(runLinkFifo_ports(3).deq.valid){
                    runLinkFifosdeqrdyReg(3):=true.B       
                    tmpuserIdReg:=runLinkFifo_ports(3).deq.bits
                    stateReg:=ischeckdata
                }.elsewhen(runLinkFifo_ports(2).deq.valid){
                    runLinkFifosdeqrdyReg(2):=true.B       
                    tmpuserIdReg:=runLinkFifo_ports(2).deq.bits
                    stateReg:=ischeckdata 
                }.elsewhen(runLinkFifo_ports(1).deq.valid){
                    runLinkFifosdeqrdyReg(1):=true.B       
                    tmpuserIdReg:=runLinkFifo_ports(1).deq.bits
                    stateReg:=ischeckdata 
                }.elsewhen(runLinkFifo_ports(0).deq.valid){
                    runLinkFifosdeqrdyReg(0):=true.B       
                    tmpuserIdReg:=runLinkFifo_ports(0).deq.bits
                    stateReg:=ischeckdata 
                }.otherwise{
                    stateReg:=idle
                }         

        }
        is(ischeckdata){
            when(userInfomationRegs(tmpuserIdReg).valid){
                when(userInfomationRegs(tmpuserIdReg).finishpacklength<userInfomationRegs(tmpuserIdReg).currpacklength)
                {
                    InterfaceuseoutvldReg:=true.B
                    stateReg:=loaduserID
                }.elsewhen(userInfomationRegs(tmpuserIdReg).finishpacklength === userInfomationRegs(tmpuserIdReg).currpacklength){
                    runLinkFifosenqvldReg(userInfomationRegs(tmpuserIdReg).priority):=true.B
                    runLinkFifo_ports(userInfomationRegs(tmpuserIdReg).priority).enq.bits:=tmpuserIdReg
                    stateReg:=idle   
                }
            }
        }
        
        is(loaduserID){
            //将用户号已传输进去
            when(io.NetworkInterfaceuserOUT.fire){
                InterfaceuseoutvldReg:=false.B
                stateReg:=reloaduserReg
            }
        }
        is(reloaduserReg){
                when(userInfomationRegs(tmpuserIdReg).runned === false.B){                
                    userInfomationRegs(tmpuserIdReg).runned :=true.B
                    userInfomationRegs(tmpuserIdReg).finishpacklength:=userInfomationRegs(tmpuserIdReg).finishpacklength+3.U
                    stateReg:=loadlinkfifo
                }.elsewhen(userInfomationRegs(tmpuserIdReg).runned === true.B){
                    userInfomationRegs(tmpuserIdReg).finishpacklength:=userInfomationRegs(tmpuserIdReg).finishpacklength+2.U
                    stateReg:=loadlinkfifo
                }
        }
        is(loadlinkfifo){
            when(userInfomationRegs(tmpuserIdReg).finishpacklength < userInfomationRegs(tmpuserIdReg).currpacklength){
                runLinkFifosenqvldReg(userInfomationRegs(tmpuserIdReg).priority):=true.B
                runLinkFifo_ports(userInfomationRegs(tmpuserIdReg).priority).enq.bits:=tmpuserIdReg
                stateReg:=done           
            }.elsewhen(userInfomationRegs(tmpuserIdReg).finishpacklength === userInfomationRegs(tmpuserIdReg).currpacklength){
                when(userInfomationRegs(tmpuserIdReg).finishpacklength ===userInfomationRegs(tmpuserIdReg).tagpacklength ){
                    userInfomationRegs(tmpuserIdReg).valid:=false.B
                    stateReg:=done
                }.elsewhen(userInfomationRegs(tmpuserIdReg).finishpacklength < userInfomationRegs(tmpuserIdReg).tagpacklength){
                    runLinkFifosenqvldReg(userInfomationRegs(tmpuserIdReg).priority):=true.B
                    runLinkFifo_ports(userInfomationRegs(tmpuserIdReg).priority).enq.bits:=tmpuserIdReg
                    stateReg:=done  
                }
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

/*******************************************************************************************************************************/
/*******************************************************************************************************************************/

class test_transmitData2(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends Module {
    val io = IO(new test_InterfaceCCBundle)
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
 

    val runCount = RegInit(0.U(3.W)) //记录当前有多少个任务还没被执行
    io.runCount:=runCount
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

    val configReg = RegInit((new configlenRegBundle).Lit(_.data_len -> datalen.U,
                                                         _.key_len -> keylen.U,
                                                         _.cfg_len ->cfglen.U,
                                                         _.result_len->resultlen.U))


    //输出数据寄存器
    val DataOutReg = RegInit(0.U.asTypeOf(new test_UnifyfieldReg))

    //输出结果寄存器
    val DataInReg = RegInit(VecInit.fill(3)(0.U.asTypeOf(new Flit)))

    DataInReg(0).header.vc_id:=DontCare
    DataInReg(1).header.vc_id:=DontCare
    //输出任务头部信息数据
    val NetworkInterfaceDataOUTReg=RegInit(0.U.asTypeOf(new PcbReg))

    //目的地址地址保存寄存器
    val destReg = RegInit(0.U(128.W))

    //userId寄存器
    val userIDReg = RegInit(0.U(3.W))


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

            when(userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).currpacklength % 2.U===0.U){
                InterfaceuseroutvldReg:=true.B
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

            when(userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).currpacklength === userInfomationRegs(io.NetworkInterfaceDataIN.bits.header.ID).tagpacklength-1.U ){
                    runCount:=runCount+1.U
             }
            
         }
    }


    when(runCount === 4.U){
        InterfaceDataInrdyReg:=false.B
    }

    when(runCount < 4.U){
        InterfaceDataInrdyReg:=true.B
    }


    //Interface状态机
    val idle ::loadcfg::loadkey :: loaddata  :: waitresult ::transmitresut::done ::Nil = Enum(7)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when(io.NetworkInterfaceuserIn.fire){
                runnedReg:=true.B //表示当前有任务正在进行
                userIDReg:=io.NetworkInterfaceuserIn.bits  //当前任务处理的ID
                when(userInfomationRegs(io.NetworkInterfaceuserIn.bits).runned){

                    configReg.cfg_len:=cfglen.asUInt
                    configReg.data_len:=datalen.asUInt
                    configReg.key_len:=0.U
                    configReg.result_len:=resultlen.asUInt

                    userportsrdys(io.NetworkInterfaceuserIn.bits):=true.B
                    stateReg:=loadcfg
                }.elsewhen(userInfomationRegs(io.NetworkInterfaceuserIn.bits).runned === false.B){
                    configReg.cfg_len:=cfglen.asUInt
                    configReg.data_len:=datalen.asUInt
                    configReg.key_len:=keylen.asUInt
                    configReg.result_len:=resultlen.asUInt 

                    userportsrdys(io.NetworkInterfaceuserIn.bits):=true.B
                    stateReg:=loadcfg
                }
            }
        }
        is(loadcfg){
            when(configReg.cfg_len =/= 0.U){
                when(usermemeoryqueues_ports(userIDReg).deq.fire){
                    DataOutReg.Address:=cfglen.asUInt-configReg.cfg_len
                    DataOutReg.flag:=0.U
                    DataOutReg.data:=usermemeoryqueues_ports(userIDReg).deq.bits.load.asTypeOf(new DataFlitLoad).data

                    DataOutVldReg:=true.B 
                }
                when(io.DataOut.fire){
                    DataOutVldReg:=false.B
                    userInfomationRegs(userIDReg).finishpacklength:=userInfomationRegs(userIDReg).finishpacklength+1.U
                    configReg.cfg_len:=configReg.cfg_len-1.U
                }
            }.otherwise{
                when(configReg.key_len =/=0.U){
                    userportsrdys(userIDReg):=true.B
                    stateReg:=loadkey
                }.otherwise{
                    when(configReg.data_len =/= 0.U){
                        userportsrdys(userIDReg):=true.B
                        stateReg:=loaddata
                    }.otherwise{
                        stateReg:=idle
                    }
                }
            }             
        }
        is(loadkey){
            when(configReg.key_len =/= 0.U){
                when(usermemeoryqueues_ports(userIDReg).deq.fire){

                    DataOutReg.Address:=keylen.asUInt-configReg.key_len
                    DataOutReg.flag:=1.U
                    DataOutReg.data:=usermemeoryqueues_ports(userIDReg).deq.bits.load.asTypeOf(new DataFlitLoad).data 

                    DataOutVldReg:=true.B                 
                }
                when(io.DataOut.fire){
                    DataOutVldReg:=false.B  
                    userInfomationRegs(userIDReg).finishpacklength:=userInfomationRegs(userIDReg).finishpacklength+1.U
                    configReg.key_len:=configReg.key_len-1.U                
                }
            }
            when(configReg.key_len === 0.U){
                userportsrdys(userIDReg):=true.B
                when(configReg.data_len =/=0.U){
                    stateReg:=loaddata
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
                    userInfomationRegs(userIDReg).finishpacklength:=userInfomationRegs(userIDReg).finishpacklength+1.U
                    when(configReg.data_len === 1.U){
                        DataOutVldReg:=false.B    
                        configReg.data_len:=configReg.data_len - 1.U  
                    }.otherwise{
                        userportsrdys(userIDReg):=true.B
                        DataOutVldReg:=false.B 
                        configReg.data_len:=configReg.data_len - 1.U  
                    }          
                }
            }
            when(configReg.data_len === 0.U){
                DataInReg(0).header.flit_type:=FlitTypes.head
                DataInReg(0).header.ID:=userIDReg
                DataInReg(0).header.currpackagelength:=2.U   //result_len+1
                DataInReg(0).header.tagpackalength:=userInfomationRegs(userIDReg).tagpacklength
                DataInReg(0).header.priority:=userInfomationRegs(userIDReg).priority
                DataInReg(0).load:= userInfomationRegs(userIDReg).config                       //配置空包
                //DataInReg(0).header.vc_id:=userInfomationRegs(userIDReg).vc_id
                DataInRdyReg:=true.B
                stateReg:=waitresult
            }
        }
        is(waitresult){
            when(configReg.result_len =/= 0.U){
                when(io.DataIn.fire){
                    DataInReg(1).header.flit_type:=FlitTypes.tail   //将结果缓存下来
                    DataInReg(1).header.ID:=userIDReg
                    DataInReg(1).header.currpackagelength:=2.U   //result_len+1
                    DataInReg(1).header.tagpackalength:=userInfomationRegs(userIDReg).tagpacklength
                    DataInReg(1).header.priority:=userInfomationRegs(userIDReg).priority
                    DataInReg(1).load:=io.DataIn.bits.data


                    DataInRdyReg:=false.B
                    InterfaceDataOutvldReg:=true.B 
                }
                when(io.NetworkInterfaceDataOUT.fire){
                    InterfaceDataOutvldReg:=false.B  
                    DataInRdyReg:=true.B       
                    configReg.result_len:=configReg.result_len-1.U            
                }
            }.otherwise{
                 DataInRdyReg:=false.B  
                 InterfaceDataOutvldReg:=true.B 
                 DataInReg(0):=DataInReg(1)
                 stateReg:=transmitresut
            }
        }
        is(transmitresut){
            when(io.NetworkInterfaceDataOUT.fire){
                InterfaceDataOutvldReg:=false.B  
                stateReg:=done      

                runCount:=runCount-1.U 
            }
        }
        is(done){
            when(userInfomationRegs(userIDReg).finishpacklength < userInfomationRegs(userIDReg).tagpacklength){ 
                configReg.cfg_len:=cfglen.asUInt
                configReg.data_len:=datalen.asUInt
                configReg.key_len:=keylen.asUInt
                configReg.result_len:=resultlen.asUInt

                userInfomationRegs(userIDReg).runned:=true.B
                InterfaceuserinrdyReg:=true.B 
                runnedReg:=false.B
                stateReg:=idle
            }.elsewhen(userInfomationRegs(userIDReg).finishpacklength === userInfomationRegs(userIDReg).tagpacklength){
                configReg.cfg_len:=cfglen.asUInt
                configReg.data_len:=datalen.asUInt
                configReg.key_len:=keylen.asUInt
                configReg.result_len:=resultlen.asUInt
                // runCount:=runCount-1.U
                userInfomationRegs(userIDReg).valid:=false.B
                InterfaceuserinrdyReg:=true.B 
                runnedReg:=false.B
                stateReg:=idle
            }             
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


    DataInReg(0)<>io.NetworkInterfaceDataOUT.bits

    io.DataOut.valid:=DataOutVldReg
    io.DataIn.ready:=DataInRdyReg
    io.runned:=runnedReg

} 

class TopSM4test(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends Module{
    val io = IO(new Bundle {
    val NetworkInterfaceDataIN = Flipped(Decoupled(new Flit))
    val NetworkInterfaceDataOUT = Decoupled(new Flit)
    val runCount = UInt(3.W)
    })
    val CC = Module(new Centralcontrol)

    val sm4 = Module(new uintSM4)
    val transmitdata = Module(new test_transmitData2(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int))
    val queue_in = Module(new Queue(new Flit,16))
    val queue_out = Module(new Queue(new Flit,16))

    io.NetworkInterfaceDataIN<>queue_in.io.enq 
    queue_in.io.deq<>transmitdata.io.NetworkInterfaceDataIN

    queue_out.io.enq<>transmitdata.io.NetworkInterfaceDataOUT
    io.NetworkInterfaceDataOUT<>queue_out.io.deq   

    transmitdata.io.NetworkInterfaceuserOUT<>CC.io.NetworkInterfaceuserIn
    CC.io.NetworkInterfaceuserOUT<>transmitdata.io.NetworkInterfaceuserIn
    CC.io.runned:=transmitdata.io.runned

    transmitdata.io.DataOut<>sm4.io.DataIn
    transmitdata.io.DataIn<>sm4.io.DataOut
    io.runCount:=transmitdata.io.runCount
}




class tesSM4CCnetworkadaptor(inPorts:Int) extends  Module {
    val io = IO(new NetworkInterfaceArbiterBundle_Top(inPorts))
    val trans_SM4 = Module(new TopSM4test(1,1,1,1))
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
    test_module.io.output<>trans_SM4.io.NetworkInterfaceDataIN
    trans_SM4.io.NetworkInterfaceDataOUT<>top_queue.io.enq
    top_queue.io.deq<>io.output

    io.runCount:=trans_SM4.io.runCount


}




class Top_transmit extends  Module {
    val io = IO(new test_InterfaceBundle)
    val trans_aes = Module(new test_transmitData1(1,1,1,1))
    val Networkinqueue = Module(new Queue(new Flit, 16))
    val Networkoutqueue = Module(new Queue(new Flit, 16))

    val Dataoutqueue = Module(new Queue(new Unifypckage, 16))
    val Dataoinqueue = Module(new Queue(new Unifypckage, 16))

    io.NetworkInterfaceDataIN<>Networkinqueue.io.enq
    Networkinqueue.io.deq<>trans_aes.io.NetworkInterfaceDataIN


    trans_aes.io.DataOut<>Dataoutqueue.io.enq
    Dataoutqueue.io.deq<>io.DataOut

    trans_aes.io.NetworkInterfaceDataOUT<>Networkoutqueue.io.enq
    Networkoutqueue.io.deq<>io.NetworkInterfaceDataOUT

    Dataoinqueue.io.deq<>trans_aes.io.DataIn
    io.DataIn<>Dataoinqueue.io.enq

}


class SM4CCnetworkadaptor(inPorts:Int) extends  Module {
    val io = IO(new NetworkInterfaceArbiterBundle_Top(inPorts))
    val trans_SM4 = Module(new TopSM4test(1,1,1,1))
    val test_module = Module(new networkModule1(inPorts))


    for(i<-0 until inPorts){
        io.inputs(i)<>test_module.io.inputs(i)
    }
    test_module.io.output<>trans_SM4.io.NetworkInterfaceDataIN

    trans_SM4.io.NetworkInterfaceDataOUT<>io.output
}


