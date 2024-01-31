package  NetworkInterface
import mesh_network._
import Interline._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import scala.util.Random

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.freespec.AnyFreeSpec
import chisel3.experimental.BundleLiterals._
import treadle.executable.Big
import org.scalatest._
import scala.util.Random
import chisel3.util._

import org.scalatest._
class SM3_topWrappertest extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new SM3_topWrapper).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>     
        dut.clock.step()
        dut.reset.poke(true.B)
        dut.clock.step(20)
        dut.io.SM3_en_in.poke(true)  
        dut.clock.step(2)   
        dut.io.msg_valid_in.poke(true)
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16)) 
        dut.clock.step()
        dut.io.msg_in.poke(BigInt("61626364",16))       
        dut.io.is_last_word_in.poke(true.B)
        dut.io.last_word_byte_in.poke(3.U)
        dut.clock.step()
        dut.io.msg_in.poke(0.U)  
        dut.io.last_word_byte_in.poke(0.U)
        dut.io.is_last_word_in.poke(false.B)
        dut.io.msg_valid_in.poke(false)
        while(!dut.io.sm3_finished_out.peekBoolean()){
          dut.clock.step()
        }
        dut.clock.step()
        dut.io.SM3_en_in.poke(false)  
        dut.clock.step(100)

     }
    }
}


class unique_SM3Moduletest extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new unique_SM3Module).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>     
        dut.clock.step(5)
        dut.io.DataIn.valid.poke(true)
        dut.io.DataIn.bits.field.flag.poke(0)
        dut.io.DataIn.bits.data.poke(2)
        dut.clock.step()  
        dut.io.DataIn.valid.poke(false)     
        dut.clock.step()  
        dut.io.DataIn.valid.poke(true)  
        dut.io.DataIn.bits.field.flag.poke(2)
        dut.io.DataIn.bits.data.poke(BigInt("61626364616263646162636461626364",16)) 
        dut.clock.step() 
        dut.io.DataIn.valid.poke(false)  
        dut.clock.step()  
        dut.io.DataIn.valid.poke(true)  
        dut.io.DataIn.bits.field.flag.poke(2)
        dut.io.DataIn.bits.data.poke(BigInt("61626364616263646162636461626364",16)) 
        dut.clock.step()
        dut.io.DataIn.valid.poke(false)    
        dut.clock.step(50)   
        dut.io.DataOut.ready.poke(true) 
        dut.clock.step(100) 
     }
    }
}

// class unique_SM3Moduletest extends AnyFlatSpec with ChiselScalatestTester {
//   "Waveform" should "pass" in {
//     test(new unique_SM3Module).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>     
//         dut.clock.step(5)
//         dut.io.input.valid.poke(true)
//         dut.io.input.bits.poke(0.U)
//         dut.clock.step()   
//         dut.io.input.valid.poke(false)  
//         dut.clock.step()  
//         dut.io.input.valid.poke(true)
//         dut.io.input.bits.poke(BigInt("61626300",16))     
//         dut.clock.step()  
//         dut.io.input.valid.poke(false)  
//         dut.clock.step(50) 
//         dut.io.output.ready.poke(true)
//         dut.clock.step(50) 
//      }
//     }
// }


// class Top_SM3networkadaptortest extends AnyFlatSpec with ChiselScalatestTester {
//   "Waveform" should "pass" in {
//     test(new Top_SM3networkadaptor(2)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>   
//         dut.io.inputs(1).valid.poke(true)
//         dut.io.inputs(1).bits.header.flit_type.poke(FlitTypes.head)
//         dut.io.inputs(1).bits.header.priority.poke(2.U)
//         dut.io.inputs(1).bits.header.vc_id.poke(0.U)
//         dut.io.inputs(1).bits.load.poke(BigInt("2",16))
//         dut.clock.step() 
//         dut.io.inputs(1).bits.header.flit_type.poke(FlitTypes.body)
//         dut.io.inputs(1).bits.header.priority.poke(2.U)
//         dut.io.inputs(1).bits.header.vc_id.poke(0.U)
//         dut.io.inputs(1).bits.load.poke(BigInt("0f0e0d0c0b0a09080706050403020100",16))     
//         dut.clock.step() 
//         dut.io.inputs(1).bits.header.flit_type.poke(FlitTypes.tail)
//         dut.io.inputs(1).bits.header.priority.poke(2.U)
//         dut.io.inputs(1).bits.header.vc_id.poke(0.U) 
//         dut.io.inputs(1).bits.load.poke(BigInt("0f0e0d0c0b0a09080706050403020100",16))
//         dut.clock.step() 
//         dut.io.inputs(1).valid.poke(false)
//         dut.clock.step()  
//         dut.io.output.ready.poke(true)
//         dut.clock.step(200)   
//      }
//     }
// }

class Top_Topsm3testtest extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new Topsm3test(1,0,2,2)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>   
      dut.clock.step(5)
      dut.io.NetworkInterfaceDataIN.valid.poke(true)
      dut.io.NetworkInterfaceDataIN.bits.load.poke(4.U)
      dut.clock.step()
      dut.io.NetworkInterfaceDataIN.bits.load.poke(BigInt("61626364616263646162636461626364",16))   
      dut.clock.step()
      dut.io.NetworkInterfaceDataIN.bits.load.poke(BigInt("61626364616263646162636461626364",16))    
      dut.clock.step() 
      dut.io.NetworkInterfaceDataIN.valid.poke(false)
      dut.clock.step()
      dut.io.NetworkInterfaceDataOUT.ready.poke(true)
      dut.clock.step(100)             
     }
    }
}


class Top_SM3networkadaptortest extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new Top_SM3networkadaptor(4)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>   
        founction_2




        def founction_1 ={
            VC_Headpoke(0,3.U,0.U)
            dut.clock.step() 
            VC_bodypoke(0,3.U,0.U)
            VC_Headpoke(1,3.U,1.U)       
            dut.clock.step()
            VC_tailpoke(0,3.U,0.U)
            VC_bodypoke(1,3.U,1.U)
            dut.clock.step() 
            dut.io.inputs(0).valid.poke(false)
            VC_tailpoke(1,3.U,1.U) 
            VC_Headpoke(2,3.U,2.U)
            dut.clock.step()   
            dut.io.inputs(1).valid.poke(false)
            VC_bodypoke(2,3.U,2.U)    
            dut.clock.step() 
            VC_tailpoke(2,3.U,2.U)   
            dut.clock.step()  
            dut.io.inputs(2).valid.poke(false)              
            dut.io.output.ready.poke(true)
            dut.clock.step(400)          
        }

        def founction_2 ={
            VC_Headpoke(0,3.U,0.U)
            VC_Headpoke(1,3.U,1.U)
            VC_Headpoke(2,3.U,2.U)
            VC_Headpoke(3,3.U,3.U)
            dut.clock.step()
            VC_bodypoke(0,3.U,0.U) 
            VC_bodypoke(1,3.U,1.U) 
            VC_bodypoke(2,3.U,2.U) 
            VC_bodypoke(3,3.U,3.U)      
            dut.clock.step()    
            VC_tailpoke(0,3.U,0.U)       
            VC_tailpoke(1,3.U,1.U)      
            VC_tailpoke(2,3.U,2.U)       
            VC_tailpoke(3,3.U,3.U)         
            dut.clock.step() 
            dut.io.inputs(0).valid.poke(false)
            dut.io.inputs(1).valid.poke(false)    
            dut.io.inputs(2).valid.poke(false)
            dut.io.inputs(3).valid.poke(false)  
            dut.clock.step()  
            dut.io.output.ready.poke(true)
            dut.clock.step(600)                                                                   
        }



        def VC_Headpoke(select:Int,priority:UInt,vc_id:UInt) = {
          dut.io.inputs(select).valid.poke(true)
          dut.io.inputs(select).bits.header.flit_type.poke(FlitTypes.head)
          dut.io.inputs(select).bits.header.priority.poke(priority)
          dut.io.inputs(select).bits.header.vc_id.poke(vc_id)
          dut.io.inputs(select).bits.load.poke(BigInt("2",16))          
        }

        def VC_bodypoke(select:Int,priority:UInt,vc_id:UInt) = {
          dut.io.inputs(select).bits.header.flit_type.poke(FlitTypes.body)
          dut.io.inputs(select).bits.header.priority.poke(priority)
          dut.io.inputs(select).bits.header.vc_id.poke(vc_id)
          dut.io.inputs(select).bits.load.poke(BigInt("61626364616263646162636461626364",16))          
        }

        def VC_tailpoke(select:Int,priority:UInt,vc_id:UInt) = {
          dut.io.inputs(select).bits.header.flit_type.poke(FlitTypes.tail)
          dut.io.inputs(select).bits.header.priority.poke(priority)
          dut.io.inputs(select).bits.header.vc_id.poke(vc_id)
          dut.io.inputs(select).bits.load.poke(BigInt("61626364616263646162636461626364",16))       
        }

     }
    }
}




class Top_SM3CCnetworkadaptortest extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new Top_sm3CCnetworkadaptor(4)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>  
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

       val flit3s = new TestPacket(1,(0,0),(0,1),2,1,
        load=Array(BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  

        val flit4s = new TestPacket(1,(0,0),(0,1),2,2,
        load=Array(BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  


       val flit5s = new TestPacket(1,(0,0),(0,1),2,1,
        load=Array(BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  

        val flit6s = new TestPacket(1,(0,0),(0,1),2,2,
        load=Array(BigInt("0f0e0d0c0b0a09080706050403020100",16),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  

      dut.io.inputs(0).valid.poke(true)
      dut.io.inputs(1).valid.poke(true)
      dut.io.inputs(2).valid.poke(true)
      dut.io.inputs(3).valid.poke(true)
      helper.userpokeFlit(dut.io.inputs(0), flit3s(0),0,6,1,2,2)
      helper.userpokeFlit(dut.io.inputs(1), flit4s(0),1,2,1,2,2)
      helper.userpokeFlit(dut.io.inputs(2), flit5s(0),2,3,1,2,2)
      helper.userpokeFlit(dut.io.inputs(3), flit6s(0),3,4,1,2,2)
      dut.clock.step()
      helper.userpokeFlit(dut.io.inputs(0), flit3s(1), 0,6,1,2,2)
      helper.userpokeFlit(dut.io.inputs(1), flit4s(1), 1,2,1,2,2)
      helper.userpokeFlit(dut.io.inputs(2), flit5s(1),2,3,1,2,2)
      helper.userpokeFlit(dut.io.inputs(3), flit6s(1),3,4,1,2,2)
      dut.clock.step()
      dut.io.inputs(0).valid.poke(false)
      dut.io.inputs(1).valid.poke(false)
      dut.io.inputs(2).valid.poke(false)
      dut.io.inputs(3).valid.poke(false)
      dut.clock.step()
      dut.io.output.ready.poke(true)
      dut.clock.step(1000)

     }
    }
}


class Top_SM3CCnetworkadaptor512test extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new Top_sm3CCnetworkadaptor512(4)).withAnnotations(Seq(VerilatorBackendAnnotation,WriteVcdAnnotation)) { dut =>  
        import NetworkConfig._
      dut.clock.setTimeout(1000000)
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
      dut.clock.step(900000)

     }
    }
}
