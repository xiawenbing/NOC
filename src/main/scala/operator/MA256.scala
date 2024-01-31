package operator

import chisel3._
import chisel3.util._

class MA256 extends Module{
    val io=IO(new UnifyBundle)
    /*
    outputDataReg : 输出io与DataReg连接
    outputfieldDataReg:field寄存器存储数据

    DataInRdyReg : 寄存器与输入io的ready信号连接
    DataOutVldReg ： 寄存器与输出io的valid 信号连接
    */
    val outputfieldDataReg = RegInit(0.U.asTypeOf(new UnifyfieldReg))

    val DataInRdyReg = RegInit(true.B)
    val DataOutVldReg = RegInit(false.B)

    val DataReg = RegInit(VecInit.fill(4)(0.U(128.W)))//数据寄存器，在接收数据中使用
    val CfgReg = RegInit(0.U(128.W))//配置寄存器，在配置IP中使用

    io.DataIn.ready:=DataInRdyReg
    io.DataOut.valid:=DataOutVldReg

    val inputdatanumcntReg = RegInit(4.U(4.W)) //输入数据长度寄存器
    val outputdatanumcntReg = RegInit(2.U(4.W)) //输出数据长度寄存器
    
    val idle::configuration::loadData::run::finish :: Nil= Enum(5)
    val stateReg = RegInit(idle)
    switch(stateReg){
        is(idle){
            when(io.DataIn.fire){
                when(io.DataIn.bits.field.flag.asUInt === 0.U(2.W)){
                    DataInRdyReg:=false.B
                    CfgReg:=io.DataIn.bits.data
                    stateReg:=configuration
                }.otherwise{
                    stateReg:=idle
                }
            }           
        }
        is(configuration){
            DataInRdyReg:=true.B           
            stateReg:=loadData
        }
        is(loadData){
             when(io.DataIn.fire){
                when(io.DataIn.bits.field.flag === 2.U(2.W)){
                    inputdatanumcntReg:=inputdatanumcntReg-1.U
                    DataReg(io.DataIn.bits.field.Address.asUInt):=io.DataIn.bits.data
                }.otherwise{
                    stateReg:=loadData
                }
            }
            when(inputdatanumcntReg === 0.U){
                DataInRdyReg:=false.B  
                stateReg:=run
            }             
        }
        is(run){
            outputfieldDataReg.Address:=0.U
            outputfieldDataReg.flag:=2.U(2.W)
            outputfieldDataReg.data:=DataReg(0)+DataReg(1)
            DataOutVldReg:=true.B
            stateReg:=finish
        }
        is(finish){
            when(io.DataOut.fire){
                outputdatanumcntReg:=outputdatanumcntReg-1.U
                outputfieldDataReg.Address:=1.U
                outputfieldDataReg.flag:=2.U(2.W)
                outputfieldDataReg.data:=DataReg(2)+DataReg(3)
            }
            when(outputdatanumcntReg === 0.U){
                DataInRdyReg:=true.B
                DataOutVldReg:=false.B
                stateReg:=idle
            }
        }
    }
    io.DataOut.bits.data:=outputfieldDataReg.data
    io.DataOut.bits.field:=outputfieldDataReg
}
