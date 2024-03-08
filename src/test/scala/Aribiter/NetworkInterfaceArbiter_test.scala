package NetworkInterfaceArbiter
import chisel3.util._
import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.BundleLiterals._
import treadle.executable.Big
import org.scalatest._
import scala.util.Random
import mesh_network._
import NetworkInterface._




class TopnetworkModule0_test extends AnyFlatSpec with ChiselScalatestTester {
  "Waveform" should "pass" in {
    test(new TopnetworkModule1(4)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>     
      def enqFlit(port: DecoupledIO[Flit], flit: soft_Flit, vc_id: Int) = {
        port.valid.poke(true)
        helper.pokeFlit(port, flit, vc_id)
        dut.clock.step()
        port.valid.poke(false)
      }
     //packet_id: Int, source: (Int, Int), dest: (Int, Int), length: Int, task_id: Int, priority: Int, currpackagelength: Int, tagpackalength: Int,
      //   val flit1s = new TestPacket(1,(0,0),(0,1),3,1,1,3,3,
      //   load=Array(BigInt(0),BigInt("11111111111111111111111",16),BigInt("2222222222222222222222222",16))).toFlits  
      //   val flit3s = new TestPacket(1,(0,0),(0,1),2,1,1,2,5,
      //   load=Array(BigInt(0),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  

      //   val flit4s = new TestPacket(2,(0,0),(0,1),2,2,2,2,5,
      //   load=Array(BigInt(0),BigInt("0f0e0d0c0b0a09080706050403020100",16))).toFlits  
      // flit3s.foreach { flit =>
      //   println(s"Flit Type: ${flit.flit_type}, Source: ${flit.source}, Dest: ${flit.dest}, Packet Type: ${flit.packet_type}, Task ID: ${flit.task_id}, currtacklength:${flit.currpackagelength},tagleng:${flit.tagpackalength},Priority: ${flit.priority}, Load: ${flit.load}")
      // }

      // val flit2s = new TestPacket(1,(0,0),(0,1),3,2,2,3,3,
      // load=Array(BigInt(0),BigInt("33333333333333333333333",16),BigInt("4444444444444444444444444",16))).toFlits  
      // flit2s.foreach { flit =>
      //   println(s"Flit Type: ${flit.flit_type}, Source: ${flit.source}, Dest: ${flit.dest}, Packet Type: ${flit.packet_type}, Task ID: ${flit.task_id}, currtacklength:${flit.currpackagelength},tagleng:${flit.tagpackalength},Priority: ${flit.priority}, Load: ${flit.load}")
      // }


    //   enqFlit(dut.io.inputs(0),flit1s(0),0)
    //  // enqFlit(dut.io.inputs(1),flit2s(0),1)
    //   enqFlit(dut.io.inputs(0),flit1s(1),0)
    //   //enqFlit(dut.io.inputs(1),flit2s(1),1)        
    //   enqFlit(dut.io.inputs(0),flit1s(2),0)   
    //   //enqFlit(dut.io.inputs(1),flit2s(2),1)

    //   dut.io.output.ready.poke(true)
    //   dut.clock.step(100)

        dut.io.inputs(0).valid.poke(true)
        dut.io.inputs(1).valid.poke(true)
        dut.io.inputs(0).bits.header.flit_type.poke(FlitTypes.head)
        dut.io.inputs(0).bits.header.priority.poke(3.U)
        dut.io.inputs(0).bits.header.vc_id.poke(0.U)
        dut.io.inputs(0).bits.header.ID.poke(0.U)
        dut.io.inputs(0).bits.header.currpackagelength.poke(3.U)
        dut.io.inputs(0).bits.header.tagpackalength.poke(4.U)
        dut.io.inputs(0).bits.load.poke(BigInt("11111111111111111111111111",16))
        dut.io.inputs(1).bits.header.flit_type.poke(FlitTypes.head)
        dut.io.inputs(1).bits.header.priority.poke(3.U)
        dut.io.inputs(1).bits.header.vc_id.poke(1.U)
        dut.io.inputs(1).bits.header.ID.poke(1.U)
        dut.io.inputs(1).bits.header.currpackagelength.poke(3.U)
        dut.io.inputs(1).bits.header.tagpackalength.poke(3.U)
        dut.io.inputs(1).bits.load.poke(BigInt("444444444444444444444444444",16))
        dut.clock.step() 
        dut.io.inputs(0).bits.header.flit_type.poke(FlitTypes.body)
        dut.io.inputs(0).bits.header.priority.poke(3.U)
        dut.io.inputs(0).bits.header.vc_id.poke(0.U)
        dut.io.inputs(0).bits.load.poke(BigInt("22222222222222222222222222",16))     
        dut.io.inputs(1).bits.header.flit_type.poke(FlitTypes.body)
        dut.io.inputs(1).bits.header.priority.poke(3.U)
        dut.io.inputs(1).bits.header.vc_id.poke(1.U)
        dut.io.inputs(1).bits.load.poke(BigInt("555555555555555555555555555",16))   
        dut.clock.step() 
        dut.io.inputs(0).bits.header.flit_type.poke(FlitTypes.tail)
        dut.io.inputs(0).bits.header.priority.poke(3.U)
        dut.io.inputs(0).bits.header.vc_id.poke(0.U) 
        dut.io.inputs(0).bits.load.poke(BigInt("33333333333333333333333333",16))
        dut.io.inputs(1).bits.header.flit_type.poke(FlitTypes.tail)
        dut.io.inputs(1).bits.header.priority.poke(3.U)
        dut.io.inputs(1).bits.header.vc_id.poke(1.U) 
        dut.io.inputs(1).bits.load.poke(BigInt("666666666666666666666666666",16))
        dut.clock.step() 
        dut.io.inputs(0).bits.header.flit_type.poke(FlitTypes.head)
        dut.io.inputs(0).bits.header.priority.poke(3.U)
        dut.io.inputs(0).bits.header.vc_id.poke(0.U)
        dut.io.inputs(0).bits.header.ID.poke(0.U)
        dut.io.inputs(0).bits.header.currpackagelength.poke(2.U)
        dut.io.inputs(0).bits.header.tagpackalength.poke(4.U)
        dut.io.inputs(0).bits.load.poke(BigInt("aaaaaaaaaaaaaaaaaaaaaaaaaaa",16))
        dut.io.inputs(1).valid.poke(false)
        dut.clock.step() 
        dut.io.inputs(0).bits.header.flit_type.poke(FlitTypes.tail)
        dut.io.inputs(0).bits.header.priority.poke(3.U)
        dut.io.inputs(0).bits.header.vc_id.poke(0.U) 
        dut.io.inputs(0).bits.load.poke(BigInt("bbbbbbbbbbbbbbbbbbbbbbbbbbb",16))       
        dut.clock.step() 
        dut.io.inputs(0).valid.poke(false)
        dut.clock.step() 
        dut.io.output.ready.poke(true)
        dut.clock.step(100) 
    }
  }
}

