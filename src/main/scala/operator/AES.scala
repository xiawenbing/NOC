package operator

import chisel3._
import chisel3.util._

class AES extends Module{
    val io=IO(new UnifyBundle)

    /*
    DataReg : 输出io与DataReg连接
    DataInRdyReg : 寄存器与输入io的ready信号连接
    DataOutVldReg ： 寄存器与输出io的valid 信号连接
    */

    val outputfieldDataReg = RegInit(0.U.asTypeOf(new UnifyfieldReg))
    val DataInRdyReg = RegInit(true.B)
    val DataOutVldReg = RegInit(false.B)
    io.DataIn.ready:=DataInRdyReg
    io.DataOut.valid:=DataOutVldReg
    val cfwDataReg = RegInit(0.U.asTypeOf(new AesSm4RegBundle))


    //密钥扩展计数器
    val ketcntReg = RegInit(30.U(8.W))
    //加解密计数器
    val opertorcntReg = RegInit(30.U(8.W))

    val idle::function::loadkey::keyExtension::loadData::deorenoperator::finish :: Nil= Enum(7)
    val stateReg = RegInit(idle)

    switch(stateReg){
        is(idle){
            when(io.DataIn.fire){
                when(io.DataIn.bits.field.flag === 0.U(2.W)){
                    DataInRdyReg:=false.B
                    cfwDataReg.cfg:=io.DataIn.bits.data
                    stateReg:=function
                }.otherwise{
                    stateReg:=idle
                }
            }
        }
        is(function){
            stateReg:=loadkey
        }
        is(loadkey){
            DataInRdyReg:=true.B
            when(io.DataIn.fire){
                when(io.DataIn.bits.field.flag === 1.U(2.W)){
                    DataInRdyReg:=false.B   
                    cfwDataReg.key:=io.DataIn.bits.data  
                    stateReg:=keyExtension 
                }.otherwise{
                    stateReg:=loadkey
                }      
            }
        }
        is(keyExtension){
            when(ketcntReg =/= 0.U){
                ketcntReg:= ketcntReg - 1.U
            }.otherwise{
                DataInRdyReg:=true.B
                stateReg:=loadData
            }
        }
        is(loadData){
            when(io.DataIn.fire){
                when(io.DataIn.bits.field.flag === 2.U){
                    cfwDataReg.data:=io.DataIn.bits.data
                    stateReg:=deorenoperator
                }
                // //指令驱动语句
                // when(io.DataIn.bits.field.flag === 3.U && io.DataIn.bits.data === 1.U ){
                //     DataInRdyReg:=false.B
                //     stateReg:=deorenoperator
                // }
            }
        }
        is(deorenoperator){
            DataInRdyReg:=false.B
            when(opertorcntReg =/= 0.U){
                opertorcntReg:=opertorcntReg-1.U
            }.otherwise{
                DataOutVldReg:=true.B
                outputfieldDataReg.data:=cfwDataReg.key+cfwDataReg.data
                outputfieldDataReg.flag:=2.U(2.W)
                outputfieldDataReg.Address:=0.U(4.W)
                stateReg:=finish
            }           
        }
        is(finish){
            when(io.DataOut.fire){
                DataInRdyReg:=true.B
                DataOutVldReg:=false.B
                ketcntReg := 30.U
                opertorcntReg:=30.U
                stateReg:=idle
            }
        }
    }
    io.DataOut.bits.data:=outputfieldDataReg.data
    io.DataOut.bits.field.Address:=outputfieldDataReg.Address
    io.DataOut.bits.field.flag:=outputfieldDataReg.flag
}