package NetworkInterfaceArbiter

import chisel3._
import chisel3.util._
import mesh_network._
import mesh_network.FlitTypes
import os.truncate



class networkModule0(inPorts:Int) extends Module{
    val io = IO(new NetworkInterfaceArbiterBundle(inPorts)) 
    //初始化输出口      
    io.output:=DontCare
    // 优先级fifo   优先级分为4级： 0、1、2、3
    val queues = Seq.fill(4)(Module(new Queue(new Flit, 16)))
    val queues_ports = Wire(Vec(4, chiselTypeOf(queues(0).io)))
    queues_ports.zip(queues).foreach{case(m, n) =>
        m<>n.io
    }
    //仲裁器:若优先级相同则采用RR仲裁器仲裁通道
    val arbiters = Seq.fill(4)(Module(new RRArbiter(UInt(0.W),inPorts)))
    val arbiterrdyReg = RegInit(VecInit.fill(4)(true.B))
    val selectportReg = RegInit(VecInit.fill(4)(0.U(log2Ceil(inPorts).W)))

    val selectinportrdyReg = RegInit(VecInit.fill(inPorts)(false.B))

    for(i<-0 until 4){
        arbiters(i).io.out.ready:=arbiterrdyReg(i)
        arbiters(i).io.in.zip(io.inputs).foreach{case(m, n) =>
            m.bits:=0.U
            m.valid:=(n.valid && n.bits.header.priority=== i.U) 
        }
        queues_ports(i).enq.valid:=false.B
        queues_ports(i).enq.bits:=DontCare
        queues_ports(i).deq.ready:=false.B
    }
    for(i<-0 until inPorts){
        io.inputs(i).ready:=false.B
    }    

    for(i<-0 until 4){
        val idle::transmitdata::done::Nil =Enum(3)    
        val stateReg = RegInit(idle)  

        switch(stateReg){
            is(idle){
                when(arbiters(i).io.out.fire){
                    selectportReg(i):=arbiters(i).io.chosen
                    stateReg:=transmitdata
                }
            }
            is(transmitdata){
                queues_ports(i).enq<>io.inputs(selectportReg(i))
                when(io.inputs(selectportReg(i)).fire){
                    when(io.inputs(selectportReg(i)).bits.header.flit_type === FlitTypes.tail){
                        stateReg:=idle
                    } 
                }
            }
        }    
    }    

    val selectfifooutReg = RegInit(0.U.asTypeOf(new outputReg))
    when(selectfifooutReg.valid === false.B){
        when(queues_ports(3).deq.valid){
            selectfifooutReg.selet:=3.U
            selectfifooutReg.valid:=true.B            
        }.elsewhen(queues_ports(2).deq.valid){
            selectfifooutReg.selet:=2.U
            selectfifooutReg.valid:=true.B             
        }.elsewhen(queues_ports(1).deq.valid){
            selectfifooutReg.selet:=1.U
            selectfifooutReg.valid:=true.B   
        }.elsewhen(queues_ports(0).deq.valid){
            selectfifooutReg.selet:=0.U
            selectfifooutReg.valid:=true.B             
        }
    }
    when(selectfifooutReg.valid === true.B){
        when(queues_ports(selectfifooutReg.selet).deq.bits.header.flit_type === FlitTypes.tail){
            when(io.output.fire){
                selectfifooutReg.valid:=false.B
            }
        }
    }
    io.output<>queues_ports(selectfifooutReg.selet).deq
}


class networkModule1(inPorts:Int) extends Module{
    val io = IO(new NetworkInterfaceArbiterBundle(inPorts)) 
    //初始化输出口      
    io.output:=DontCare
    // 优先级fifo   优先级分为4级： 0、1、2、3
    val queues = Seq.fill(4)(Module(new Queue(new Flit, 16)))
    val queues_ports = Wire(Vec(4, chiselTypeOf(queues(0).io)))
    queues_ports.zip(queues).foreach{case(m, n) =>
        m<>n.io
    }

    //仲裁器:若优先级相同则采用RR仲裁器仲裁通道
    val arbiters = Seq.fill(4)(Module(new RRArbiter(UInt(0.W),inPorts)))
    val arbiterrdyReg = RegInit(VecInit.fill(4)(true.B))
    val selectportReg = RegInit(VecInit.fill(4)(0.U(log2Ceil(inPorts).W)))

    val selectinportrdyReg = RegInit(VecInit.fill(inPorts)(false.B))

    for(i<-0 until 4){
        arbiters(i).io.out.ready:=arbiterrdyReg(i)
        arbiters(i).io.in.zip(io.inputs).foreach{case(m, n) =>
            m.bits:=0.U
            m.valid:=(n.valid && n.bits.header.priority === i.U) 
        }  
        queues_ports(i).enq.valid:=false.B
        queues_ports(i).enq.bits:=DontCare
        queues_ports(i).deq.ready:=false.B
    }
    for(i<-0 until inPorts){
        io.inputs(i).ready:=false.B
    }    

    for(i<-0 until 4){
        val idle::transmitdata::done::Nil =Enum(3)    
        val stateReg = RegInit(idle)  

        switch(stateReg){
            is(idle){
                when(arbiters(i).io.out.fire){
                    selectportReg(i):=arbiters(i).io.chosen
                    stateReg:=transmitdata
                }
            }
            is(transmitdata){
                queues_ports(i).enq<>io.inputs(selectportReg(i))
                when(io.inputs(selectportReg(i)).fire){
                    when(io.inputs(selectportReg(i)).bits.header.flit_type === FlitTypes.tail){
                        stateReg:=idle
                    } 
                }
            }
        }    
    }    

    val selectfifooutReg = RegInit(0.U.asTypeOf(new outputReg))
    when(selectfifooutReg.valid === false.B){
        when(queues_ports(3).deq.valid){
            selectfifooutReg.selet:=3.U
            selectfifooutReg.valid:=true.B            
        }.elsewhen(queues_ports(2).deq.valid){
            selectfifooutReg.selet:=2.U
            selectfifooutReg.valid:=true.B             
        }.elsewhen(queues_ports(1).deq.valid){
            selectfifooutReg.selet:=1.U
            selectfifooutReg.valid:=true.B   
        }.elsewhen(queues_ports(0).deq.valid){
            selectfifooutReg.selet:=0.U
            selectfifooutReg.valid:=true.B             
        }
    }
    when(selectfifooutReg.valid === true.B){
        when(queues_ports(selectfifooutReg.selet).deq.bits.header.flit_type === FlitTypes.tail){
            when(io.output.fire){
                selectfifooutReg.valid:=false.B
            }
        }
    }
    io.output<>queues_ports(selectfifooutReg.selet).deq
}
















//顶层模块
class TopnetworkModule1 (inPorts:Int) extends Module{
    val io = IO(new NetworkInterfaceArbiterBundle(inPorts))
   
    val Topqueues = Seq.fill(inPorts)(Module(new Queue(new Flit, 16)))
    val Topqueues_ports = Wire(Vec(inPorts, chiselTypeOf(Topqueues(0).io)))
    Topqueues_ports.zip(Topqueues).foreach{case(m, n) =>
        m<>n.io
    }
    val test_module = Module(new networkModule0(inPorts))
    for(i<-0 until inPorts){
        Topqueues_ports(i).enq<>io.inputs(i)
        test_module.io.inputs(i)<>Topqueues_ports(i).deq
    }

    io.output<>test_module.io.output
}