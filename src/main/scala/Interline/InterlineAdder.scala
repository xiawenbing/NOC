package Interline
import chisel3._
import chisel3.util._
import operator._
import NetworkInterface._
import NetworkInterfaceArbiter._
import mesh_network._
class SM3_topIO extends Bundle {
    val clk_in = Input(Clock())
    val reset_n_in = Input(Bool())
    val SM3_en_in = Input(Bool())
    val msg_in = Input(UInt(32.W))
    val msg_valid_in = Input(Bool())
    val is_last_word_in = Input(Bool())
    val last_word_byte_in = Input(UInt(2.W))

    val sm3_result_out = Output(UInt(256.W))
    val sm3_finished_out = Output(Bool())
}

class SM3topIO extends Bundle {
    val SM3_en_in = Input(Bool())
    val msg_in = Input(UInt(32.W))
    val msg_valid_in = Input(Bool())
    val is_last_word_in = Input(Bool())
    val last_word_byte_in = Input(UInt(2.W))

    val sm3_result_out = Output(UInt(256.W))
    val sm3_finished_out = Output(Bool())
}

class SM3_top extends BlackBox with HasBlackBoxPath {
  val io = IO(new SM3_topIO).suggestName("io")  
  addPath("./src/main/resources/ResourceBlackBoxAdder.v")
  
}

class SM3_topWrapper extends Module {
  val io = IO(new SM3topIO)
  val m = Module(new SM3_top)

  m.io.SM3_en_in:=io.SM3_en_in
  m.io.clk_in:=clock
  m.io.is_last_word_in:=io.is_last_word_in
  m.io.last_word_byte_in:=io.last_word_byte_in
  m.io.msg_in:=io.msg_in
  m.io.msg_valid_in:=io.msg_valid_in
  m.io.reset_n_in:=reset
  io.sm3_finished_out:=m.io.sm3_finished_out
  io.sm3_result_out:=m.io.sm3_result_out

}

class  unique_SM3Module extends Module{
  val io = IO(new UnifyBundle)

/******************实例化sm3************************/
  //实例化Modle
  val sm3 = Module(new SM3_topWrapper)

  val sm3_reset= RegInit(false.B)  
  val sm3_en_in = RegInit(false.B)
  val is_last_word_in = RegInit(false.B)
  val msg_valid_in = RegInit(false.B)
  val msg_in = RegInit(0.U(32.W))
  val last_word_byte_in = RegInit(0.U(2.W))
  val sm3_result_out = RegInit(VecInit(Seq.fill(2)(0.U(128.W))))

  sm3.io.SM3_en_in:=sm3_en_in
  sm3.io.is_last_word_in:=is_last_word_in
  sm3.io.last_word_byte_in:=last_word_byte_in
  sm3.io.msg_in:=msg_in
  sm3.io.msg_valid_in:=msg_valid_in
  sm3.reset:=sm3_reset
  sm3_result_out:=sm3.io.sm3_result_out.asTypeOf(VecInit(Seq.fill(2)(0.U(128.W))))

  /************输入输出接口初始化************/
  val inputrdyReg = RegInit(true.B)
  val outputvld = RegInit(false.B)
  io.DataIn.ready:=inputrdyReg
  io.DataOut.valid:=outputvld
  io.DataOut.bits:=DontCare
/**************寄存器初始化****************/
  val timecnt = RegInit(0.U(6.W))  //sm3节拍器
  val cnt =  RegInit(0.U(3.W)) //计算器
  val text_length = RegInit(0.U(128.W)) //记录数据包长度
  val tmptext = RegInit (VecInit(Seq.fill(4)(VecInit(Seq.fill(4)(0.U(32.W))))))//数据缓存区
  val resultReg = RegInit(VecInit(Seq.fill(2)(0.U(128.W))))
  val seletVec = RegInit(0.U(3.W))
 /**************状态机****************/     
  //FSM 
  val loadcfg::loadtext::late1::transmitData::waitresult::transmitresult_0::transmitresult_1 ::Nil= Enum(7)
  val stateReg = RegInit(loadcfg)  
  switch(stateReg){
    is(loadcfg){
      when(io.DataIn.fire && io.DataIn.bits.field.flag === 0.U){
        text_length:=io.DataIn.bits.data
        stateReg:=loadtext
      }
    }
    is(loadtext){
      when(io.DataIn.fire && io.DataIn.bits.field.flag === 2.U){
        text_length:=text_length-1.U
        tmptext(cnt):=io.DataIn.bits.data.asTypeOf(VecInit(Seq.fill(4)(0.U(32.W))))
        cnt:=cnt+1.U
      }      
      when(text_length === 0.U){
        sm3_reset:=true.B
        inputrdyReg:=false.B
        sm3_en_in:=true.B
        stateReg:=late1
      }
    }
    is(late1){
        stateReg:=transmitData
    }
    is(transmitData){
      msg_valid_in:=true.B
      switch(cnt){
        is(1.U){
          timecnt:=timecnt+1.U
          msg_in:=tmptext(seletVec)(timecnt%4.U)
          when(timecnt === 3.U){
            is_last_word_in:=true.B
            last_word_byte_in:=11.U
            stateReg:=waitresult
          }
        }
        is(2.U){
          timecnt:=timecnt+1.U  
          msg_in:=tmptext(seletVec)(timecnt%4.U) 
          when(timecnt=/= 0.U && timecnt%4.U === 0.U){
              seletVec:=seletVec+1.U
          }
          when(timecnt===7.U){
            is_last_word_in:=true.B
            last_word_byte_in:=11.U
            stateReg:=waitresult            
          }
        }
        is(3.U){
          timecnt:=timecnt+1.U  
          msg_in:=tmptext(seletVec)(timecnt%4.U) 
          when(timecnt=/= 0.U && timecnt%4.U === 0.U){
              seletVec:=seletVec+1.U
          }
          when(timecnt===11.U){
            is_last_word_in:=true.B
            last_word_byte_in:=11.U
            stateReg:=waitresult            
          }
        }
        is(4.U){
          timecnt:=timecnt+1.U  
          msg_in:=tmptext(seletVec)(timecnt%4.U) 
          when(timecnt=/= 0.U && timecnt%4.U === 0.U){
              seletVec:=seletVec+1.U
          }
          when(timecnt===15.U){
            is_last_word_in:=true.B
            last_word_byte_in:=11.U
            stateReg:=waitresult            
          }
        }
      }
    }
    is(waitresult){
      seletVec:=0.U
      msg_valid_in:=false.B
      msg_in:=0.U
      is_last_word_in:=false.B
      last_word_byte_in:=0.U
      when(sm3.io.sm3_finished_out){
        outputvld:=true.B
        sm3_en_in:=false.B
        resultReg:=sm3.io.sm3_result_out.asTypeOf(VecInit(Seq.fill(2)(0.U(128.W))))
        stateReg:=transmitresult_0
      }
    }
    is(transmitresult_0){
      when(io.DataOut.fire){
        io.DataOut.bits.field.Address:=0.U
        io.DataOut.bits.field.flag:=2.U
        io.DataOut.bits.data:=resultReg(1)  
        stateReg:=transmitresult_1
      }
    }
    is(transmitresult_1){
      when(io.DataOut.fire){
        outputvld:=false.B 
        inputrdyReg:=true.B  
        sm3_reset:=false.B
        cnt:=0.U 
        timecnt:=0.U   
        io.DataOut.bits.field.Address:=1.U
        io.DataOut.bits.field.flag:=2.U
        io.DataOut.bits.data:=resultReg(0)  
        stateReg:=loadcfg
      }
    }
  }
}
//cfg:: 1，0，2,2
class Topsm3test(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int) extends Module{
    val io = IO(new Bundle {
    val NetworkInterfaceDataIN = Flipped(Decoupled(new Flit))
    val NetworkInterfaceDataOUT = Decoupled(new Flit)
    })
    
    val sm3 = Module(new unique_SM3Module)
    val transmitdata = Module(new test_transmitData2(cfglen:Int,keylen:Int,datalen:Int,resultlen:Int))
    val queue_in = Module(new Queue(new Flit,16))
    val queue_out = Module(new Queue(new Flit,16))

    queue_in.io.enq<>io.NetworkInterfaceDataIN
    transmitdata.io.NetworkInterfaceDataIN<>queue_in.io.deq

    queue_out.io.enq<>transmitdata.io.NetworkInterfaceDataOUT
    io.NetworkInterfaceDataOUT<>queue_out.io.deq   

    transmitdata.io.DataOut<>sm3.io.DataIn
    transmitdata.io.DataIn<>sm3.io.DataOut
}

class Top_SM3networkadaptor(inPorts:Int) extends  Module {
    val io = IO(new NetworkInterfaceArbiterBundle_Top(inPorts))
    val trans_aes = Module(new Topsm3test(1,0,2,2))
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
