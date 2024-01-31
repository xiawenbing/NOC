package clusterSM4
import mesh_network._
import operator._
import chisel3._
import chisel3.util._
import NetworkInterface._
// although the hardware for sm4 enc/dec is the same, seperating them can be helpful to
// create same interface with AES and to handle CBC mode easier
class Group(encNum: Int) extends Module {
  require(encNum >= 0 && encNum <= 4)
  val io = IO(new Bundle {
    val workID_key = Input(UInt(4.W))
    val user_key = Flipped(Decoupled(Vec(4, UInt(32.W))))

    val workID_start = Input(UInt(4.W))
    val text_in = Flipped(DecoupledIO(Vec(4, UInt(32.W))))
    
    val workID_read = Output(UInt(4.W))
    val text_out = Decoupled(Vec(4, UInt(32.W)))   
  })

  val WorkID_Key = RegInit(0.U(4.W))
  val OutArbiter = Module(new RRArbiter(Vec(4, UInt(32.W)), 4))
  
  val KeyExpansionModule = Module(new KeyExpansion)
  var encCount = 0
  val Units = Seq.fill(4)(Module(new Unit))

  def takeUnitID(workID: UInt): UInt = workID(3, 2)  //取前两位
  def takeTaskID(workID: UInt): UInt = workID(1, 0)  //取后两位

  when(io.user_key.fire) {
    WorkID_Key := io.workID_key
  }

  KeyExpansionModule.io.user_key <> io.user_key

  for(i <- 0 until 4) {
    when(takeUnitID(WorkID_Key) === i.U) {
      Units(i).io.write_en := KeyExpansionModule.io.write_round.valid
      Units(i).io.write_task := takeTaskID(WorkID_Key) //00
      Units(i).io.write_round := KeyExpansionModule.io.write_round.bits
      Units(i).io.write_key := KeyExpansionModule.io.roundKey
    }.otherwise {
      Units(i).io.write_en := false.B
      Units(i).io.write_task := 0.U
      Units(i).io.write_round := 0.U
      Units(i).io.write_key := DontCare
    }
    //takeUnitId(00)  取低i个SM4模块进行加解密
    when(takeUnitID(io.workID_start) === i.U) {
      Units(i).io.input.valid := io.text_in.valid
      Units(i).io.input.bits.state := io.text_in.bits
      Units(i).io.input.bits.control.isIdle := false.B
      val isEnc = if(i < encNum) true.B else false.B
      Units(i).io.input.bits.control.isEnc := isEnc
      Units(i).io.input.bits.control.taskID := takeTaskID(io.workID_start) //01
      Units(i).io.input.bits.control.rounds := 0.U
    }.otherwise {
      Units(i).io.input.valid := false.B
      Units(i).io.input.bits.state := DontCare
      Units(i).io.input.bits.control.isIdle := true.B
      Units(i).io.input.bits.control.isEnc := false.B
      Units(i).io.input.bits.control.taskID := 0.U
      Units(i).io.input.bits.control.rounds := 0.U
    }

    OutArbiter.io.in(i) <> Units(i).io.output
  }

  io.text_in.ready := MuxLookup(takeUnitID(io.workID_start), false.B, 
                                (0 until 4).map(i => (i.U, Units(i).io.input.ready)))
  io.text_out <> OutArbiter.io.out
  io.workID_read := Cat(OutArbiter.io.chosen, MuxLookup(OutArbiter.io.chosen, 0.U, 
                                                        (0 until 4).map(i => (i.U, Units(i).io.output_task))))
}



class TopSM4 extends Module {
  val io = IO(new Bundle {
    val work_ID = Flipped(Decoupled(UInt(2.W)))      //任务数最大为2的4次方
    val user_key = Flipped(Decoupled(Vec(4, UInt(32.W))))
    val text_in = Flipped(Decoupled(Vec(4, UInt(32.W))))
    
    val text_out = Decoupled(Vec(4, UInt(32.W)))   
  })

  val WorkID = RegInit(0.U(3.W))
  
  val KeyExpansionModule = Module(new KeyExpansion)
  val Unit = Module(new Unit)



  when(io.work_ID.fire) {
     WorkID := io.work_ID.bits
   }
      KeyExpansionModule.io.user_key <> io.user_key

      Unit.io.write_en := KeyExpansionModule.io.write_round.valid
      Unit.io.write_task := WorkID //00
      Unit.io.write_round := KeyExpansionModule.io.write_round.bits
      Unit.io.write_key := KeyExpansionModule.io.roundKey  //密钥扩展后的结果

      Unit.io.input.valid := io.text_in.valid
      Unit.io.input.bits.state := io.text_in.bits
      Unit.io.input.bits.control.isIdle := false.B
      Unit.io.input.bits.control.isEnc := true.B
      Unit.io.input.bits.control.taskID := WorkID //01
      Unit.io.input.bits.control.rounds := 0.U

      io.text_in.ready := Unit.io.input.ready
      io.text_out <> Unit.io.output
      io.work_ID.ready:=Unit.io.output.ready //任务完成后才可输入密钥
     
}


class uintSM4 extends Module {
  val io = IO(new UnifyBundle)

  //输入输出接口初始化
  val inputrdyReg = RegInit(true.B)
  io.DataIn.ready:=inputrdyReg


  val UnitTopSM4 = Module(new TopSM4)
  val temReg = RegInit(0.U.asTypeOf(new SM4Bundle))
  val loadcfg::loadkey::late::waitkeyexpand::loadtext::waitresult:: Nil= Enum(6)
  val stateReg = RegInit(loadcfg)

  //用户ID记录表

  def tagPackagelength(cfg:UInt):UInt = cfg(5,2)//目标包数
  def currPackagelength(cfg:UInt):UInt = cfg(9,6)//目标包数

      //PCB表
  val userInfomationRegs =RegInit(VecInit.fill(8)(0.U.asTypeOf(new SM4Pcb)))

  switch(stateReg){
    is(loadcfg){
      when(io.DataIn.fire && io.DataIn.bits.field.flag === 0.U){
        temReg.cfg.bits:=io.DataIn.bits.data.asTypeOf(new HeadFlitLoad).task_id  //记录ID号
        temReg.tagpackagelenth:=tagPackagelength(io.DataIn.bits.data)
        temReg.currpackagelenth:=currPackagelength(io.DataIn.bits.data)  //bug 显示不出来 tagpackageslength
        temReg.cfg.valid:=true.B
        stateReg:=loadkey
      }
    }
    is(loadkey){
      when(userInfomationRegs(temReg.cfg.bits).valid){
        userInfomationRegs(temReg.cfg.bits).currpacklength:=userInfomationRegs(temReg.cfg.bits).currpacklength+1.U
        stateReg:=loadtext
      }.otherwise{
        when(io.DataIn.fire && io.DataIn.bits.field.flag === 1.U){
          inputrdyReg:=false.B

          userInfomationRegs(temReg.cfg.bits).valid:=true.B
          userInfomationRegs(temReg.cfg.bits).ID:=temReg.cfg.bits
          userInfomationRegs(temReg.cfg.bits).tagpacklength:=temReg.tagpackagelenth

          userInfomationRegs(temReg.cfg.bits).currpacklength:=userInfomationRegs(temReg.cfg.bits).currpacklength+2.U
          temReg.key.bits:=io.DataIn.bits.data
          temReg.key.valid:=true.B
          stateReg:=late
        }
      }
    }
    is(late){
      stateReg:=waitkeyexpand
    }
    is(waitkeyexpand){
       when(UnitTopSM4.io.user_key.ready === true.B){
        inputrdyReg:=true.B
        stateReg:=loadtext
      }
    }
    is(loadtext){
      when(io.DataIn.fire && io.DataIn.bits.field.flag === 2.U){
        userInfomationRegs(temReg.cfg.bits).currpacklength:=userInfomationRegs(temReg.cfg.bits).currpacklength+1.U
        temReg.text.bits:=io.DataIn.bits.data
        temReg.text.valid:=true.B
        inputrdyReg:=false.B
      }  
      when(UnitTopSM4.io.text_in.fire){
        temReg.text.valid:=false.B
        stateReg:=waitresult
      }          
    }
    is(waitresult){
      when(io.DataOut.fire){
        when(userInfomationRegs(temReg.cfg.bits).tagpacklength === userInfomationRegs(temReg.cfg.bits).currpacklength){
          userInfomationRegs(temReg.cfg.bits).valid:=false.B
          inputrdyReg:=true.B
          stateReg:=loadcfg 
        }.otherwise{
          inputrdyReg:=true.B
          stateReg:=loadcfg           
        }      
          inputrdyReg:=true.B
          stateReg:=loadcfg 
      }
    }
  }

  UnitTopSM4.io.work_ID.bits:=temReg.cfg.bits
  UnitTopSM4.io.work_ID.valid:=temReg.cfg.valid

  UnitTopSM4.io.user_key.bits:=temReg.key.bits.asTypeOf(Vec(4,UInt(32.W)))
  UnitTopSM4.io.user_key.valid:=temReg.key.valid

  UnitTopSM4.io.text_in.bits:=temReg.text.bits.asTypeOf(Vec(4,UInt(32.W)))
  UnitTopSM4.io.text_in.valid:=temReg.text.valid  

  io.DataOut.valid:=UnitTopSM4.io.text_out.valid 
  UnitTopSM4.io.text_out.ready:=io.DataOut.ready

  
  io.DataOut.bits.data:= UnitTopSM4.io.text_out.bits.asTypeOf(UInt(128.W))
  io.DataOut.bits.field:=DontCare

  when(UnitTopSM4.io.work_ID.fire){
     temReg.cfg.valid:=false.B
 }

  when(UnitTopSM4.io.user_key.fire){
     temReg.key.valid:=false.B
 }
}