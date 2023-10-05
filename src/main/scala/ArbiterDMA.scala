package Tesla
import chisel3._
import chisel3.util._
/************************Bundle of write**********************************/
class inwrArbipackage(numlen:Int,depth:Int) extends Bundle{
    val wrAddr = UInt(log2Ceil(depth).W)
    val wrdata_length = UInt(numlen.W)
}

class schedulewrBundle(numlen: Int, depth: Int, width: Int) extends Bundle{
    val wrConfig = Flipped(DecoupledIO(new inwrArbipackage(numlen,depth)))
    val wrData = Flipped(DecoupledIO(UInt(width.W)))
}

class  outArbiwrpackage(width:Int,depth:Int) extends Bundle{
    val wrAddr = Output(UInt (log2Ceil(depth).W))
    val wrData = Output(UInt (width.W))
    val wrEna = Output(Bool ())
}

class  Arbiwrpackage(inPorts:Int,numlen:Int,width:Int,depth:Int) extends  Bundle
{
    val inputs = Vec(inPorts, new schedulewrBundle(numlen,depth,width))
    val output = new outArbiwrpackage(width,depth)//the width of output length  such as 128 bits
}
/**********************************Bundle of read******************************************/
class inrdArbipackage(numlen:Int,depth:Int) extends Bundle{
    val rdAddr = UInt (log2Ceil(depth).W)
    val rddata_length = UInt(numlen.W)
}

class schedulerdBundle(numlen: Int, depth: Int, width: Int) extends Bundle{
    val rdConfig = Flipped(DecoupledIO(new inrdArbipackage(numlen,depth)))
    val rdData = DecoupledIO(UInt(width.W))
}

class  outArbirdpackage(width:Int,depth:Int) extends Bundle{
    val rdAddr = Output(UInt (log2Ceil(depth).W))
    val rdData = Input (UInt (width.W))
}

class  Arbirdpackage(inPorts:Int,numlen:Int,depth:Int,width:Int) extends  Bundle
{
    val inputs = Vec(inPorts, new schedulerdBundle(numlen,depth,width))
    val output = new outArbirdpackage(width,depth)//the width of output length  such as 128 bits
}
/********************************************Bundle of Reg***************************************************/
class RegBundle(inPorts:Int,numlen:Int,depth:Int) extends Bundle{
    val addr = UInt(log2Ceil(depth).W)
    val data_length= UInt(numlen.W)
    val ports_numlen = UInt(log2Ceil(inPorts).W)
}
/********************************************Bundle of ArbiDMA***************************************************/

class scheduleBundle(numlen: Int, depth: Int, width: Int) extends Bundle{
    val wrConfig = Flipped(DecoupledIO(new inwrArbipackage(numlen,depth)))
    val rdConfig = Flipped(DecoupledIO(new inrdArbipackage(numlen,depth)))
    val wrData = Flipped(DecoupledIO(UInt(width.W)))
    val rdData = DecoupledIO(UInt(width.W))
}

class  Arbipackage(inPorts:Int,numlen:Int,depth:Int, width:Int) extends  Bundle
{
    val inputs = Vec(inPorts, new scheduleBundle(numlen,depth,width))
}

/********************************************Module wrschaedule********************************************/
class ArbiDMAwr(inPorts:Int,numlen:Int,depth:Int,width:Int) extends Module{
    val io = IO(new Arbiwrpackage(inPorts,numlen,width,depth))
    
    // Create an Arbiter with inPorts inputs and one output for write
    val wrarbiter = Module(new Arbiter(new inwrArbipackage(numlen, depth), inPorts))

    val wrconfigReg = RegInit(0.U.asTypeOf(new RegBundle(inPorts,numlen,depth))) //the write config lines total of 16 bits

    for(i<-0 until inPorts){
        io.inputs(i).wrConfig.ready:=wrarbiter.io.in(i).ready
        wrarbiter.io.in(i).valid:=io.inputs(i).wrConfig.valid
        wrarbiter.io.in(i).bits.wrAddr:=io.inputs(i).wrConfig.bits.wrAddr
        wrarbiter.io.in(i).bits.wrdata_length:=io.inputs(i).wrConfig.bits.wrdata_length
        io.inputs(i).wrData.ready := false.B
    }

    when(wrarbiter.io.out.fire) {
        wrconfigReg.addr:=wrarbiter.io.out.bits.wrAddr
        wrconfigReg.data_length:=wrarbiter.io.out.bits.wrdata_length
        wrconfigReg.ports_numlen:= OHToUInt(wrarbiter.io.chosen)
    }

    wrarbiter.io.out.ready := wrconfigReg.data_length === 0.U
    
    io.output.wrEna:=io.inputs(wrconfigReg.ports_numlen).wrData.fire
    io.inputs(wrconfigReg.ports_numlen).wrData.ready:= wrconfigReg.data_length =/= 0.U
    
    when(io.inputs(wrconfigReg.ports_numlen).wrData.fire){
        wrconfigReg.data_length:=wrconfigReg.data_length-1.U
    }

    io.output.wrAddr:=wrconfigReg.addr
    io.output.wrData:=io.inputs(wrconfigReg.ports_numlen).wrData.bits
}

/**************************************************************************************************/

/************************************Module rdschaedule********************************************/
class ArbiDMArd(inPorts:Int,numlen:Int,depth:Int,width:Int) extends Module{
    val io = IO(new Arbirdpackage(inPorts,numlen,width,depth))
    // Create an Arbiter with inPorts inputs and one output for write
    val rdarbiter = Module(new Arbiter(new inrdArbipackage(numlen, depth), inPorts))


    val rdconfigReg = RegInit(0.U.asTypeOf(new RegBundle(inPorts,numlen,depth))) //the read config lines total of 16 bits
    for(i<-0 until inPorts){
        io.inputs(i).rdConfig.ready:=rdarbiter.io.in(i).ready
        rdarbiter.io.in(i).valid:=io.inputs(i).rdConfig.valid
        rdarbiter.io.in(i).bits.rdAddr:=io.inputs(i).rdConfig.bits.rdAddr
        rdarbiter.io.in(i).bits.rddata_length:=io.inputs(i).rdConfig.bits.rddata_length
        io.inputs(i).rdData.valid := false.B
        io.inputs(i).rdData.bits:=DontCare
    }


    when(rdarbiter.io.out.fire) {
        rdconfigReg.addr:=rdarbiter.io.out.bits.rdAddr
        rdconfigReg.data_length:=rdarbiter.io.out.bits.rddata_length
        rdconfigReg.ports_numlen:=OHToUInt(rdarbiter.io.chosen)
    }    

 
    rdarbiter.io.out.ready:=Mux(rdconfigReg.data_length === 0.U, true.B, false.B)
    
    io.inputs(rdconfigReg.ports_numlen).rdData.valid:=rdconfigReg.data_length =/= 0.U
    when(io.inputs(rdconfigReg.ports_numlen).rdData.fire){
        rdconfigReg.data_length:=rdconfigReg.data_length-1.U
        io.inputs(rdconfigReg.ports_numlen).rdData.bits:=io.output.rdData
    }

    io.output.rdAddr:=rdconfigReg.addr
}


/**************************************************************************************************/

/************************************Module schaedule********************************************/
class ArbiDMA(inPorts:Int,numlen:Int,depth:Int,width:Int) extends Module{
    val io = IO(new Arbipackage(inPorts,numlen,width,depth))

    val wrArbi = Module(new ArbiDMAwr(inPorts,numlen,width,depth))
    val rdArbi = Module(new ArbiDMArd(inPorts,numlen,width,depth))
    val bram = Module(new share_memory(width,depth))

    // for(i<-0 until inPorts)
    // {
    //     wrArbi.io.inputs(i).wrConfig<>io.inputs(i).wrConfig
    //     wrArbi.io.inputs(i).wrData<>io.inputs(i).wrData

    //     rdArbi.io.inputs(i).rdConfig<>io.inputs(i).rdConfig
    //     io.inputs(i).rdData<>rdArbi.io.inputs(i).rdData
    // }

    wrArbi.io.inputs.zip(io.inputs).foreach{case(w, i) => 
        w.wrConfig.bits := i.wrConfig.bits
        w.wrConfig.valid := i.wrConfig.valid
        i.wrConfig.ready := w.wrConfig.ready
        w.wrData.bits := i.wrData.bits
        w.wrData.valid := i.wrData.valid
        i.wrData.ready := w.wrData.ready
    }

    rdArbi.io.inputs.zip(io.inputs).foreach{case(r, i) =>
        r.rdConfig.bits := i.rdConfig.bits
        r.rdConfig.valid := i.rdConfig.valid
        i.rdConfig.ready := r.rdConfig.ready

        i.rdData.bits := r.rdData.bits
        i.rdData.valid := r.rdData.valid
        r.rdData.ready := i.rdData.ready

    }
    
    bram.io.wrAddr:=wrArbi.io.output.wrAddr
    bram.io.wrData:=wrArbi.io.output.wrData
    bram.io.wrEna:=wrArbi.io.output.wrEna

    bram.io.rdAddr:=rdArbi.io.output.rdAddr
    rdArbi.io.output.rdData:=bram.io.rdData
}








// package Tesla
// import chisel3._
// import chisel3.util._

// /**********************************************
//   inwrArbipackage :
//     wrAddr : address length of bits such as 12
//     wrdata_length:the package count of bits such as 4

//  **********************************************/
// class inwrArbipackage(numlen:Int,depth:Int) extends Bundle{
//     val wrAddr = UInt(log2Ceil(depth).W)
//     val wrdata_length = UInt(numlen.W)
// }

// //Input rdPort
// class inrdArbipackage(numlen:Int,depth:Int) extends Bundle{
//     val rdAddr = UInt (log2Ceil(depth).W)
//     val rddata_length = UInt(numlen.W)
// }

// class schedulerBundle(numlen: Int, depth: Int, width: Int) extends Bundle{
//     val wrConfig = Flipped(DecoupledIO(new inwrArbipackage(numlen,depth)))
//     val rdConfig = Flipped(DecoupledIO(new inrdArbipackage(numlen,depth)))
//     val wrData = Flipped(DecoupledIO(UInt(width.W)))
//     val rdData = DecoupledIO(UInt(width.W))
// }

// /*******************************************************/
// //output Port
// class  outArbipackage(width:Int,depth:Int) extends Bundle{
//     val rdAddr = Output(UInt (log2Ceil(depth).W))
//     val rdData = Input (UInt (width.W))
//     val wrAddr = Output(UInt (log2Ceil(depth).W))
//     val wrData = Output(UInt (width.W))
//     val wrEna = Output(Bool ())
// }

// /**********************************************
//   RegBundle :
//     addr : read or write address
//     data_length:read or write the numlen of packages
//     ports_numlen: the numlenber of inports
//  **********************************************/
// class RegBundle(inPorts:Int,numlen:Int,depth:Int) extends Bundle{
//     val addr = UInt(log2Ceil(depth).W)
//     val data_length= UInt(numlen.W)
//     val ports_numlen = UInt(log2Ceil(inPorts).W)
// }

// class  Arbipackage(inPorts:Int,numlen:Int,width:Int,depth:Int) extends  Bundle
// {
//     val inputs = Vec(inPorts, new schedulerBundle(numlen,depth,width))
//     val output = new outArbipackage(width,depth)//the width of output length  such as 128 bits
// }


// class ArbiDMA(inPorts:Int,numlen:Int,depth:Int,width:Int) extends Module{
//     val io = IO(new Arbipackage(inPorts,numlen,width,depth))
    
//     // Create an Arbiter with inPorts inputs and one output for write
//     val wrarbiter = Module(new Arbiter(new inwrArbipackage(numlen, depth), inPorts))
//     // Create an Arbiter with inPorts inputs and one output for write
//     val rdarbiter = Module(new Arbiter(new inrdArbipackage(numlen, depth), inPorts))


//     val wrconfigReg = RegInit(0.U.asTypeOf(new RegBundle(inPorts,numlen,depth))) //the write config lines total of 16 bits
//     val rdconfigReg = RegInit(0.U.asTypeOf(new RegBundle(inPorts,numlen,depth))) //the read config lines total of 16 bits
//     for(i<-0 until inPorts){
//         io.inputs(i).wrConfig.ready:=wrarbiter.io.in(i).ready
//         wrarbiter.io.in(i).valid:=io.inputs(i).wrConfig.valid
//         wrarbiter.io.in(i).bits.wrAddr:=io.inputs(i).wrConfig.bits.wrAddr
//         wrarbiter.io.in(i).bits.wrdata_length:=io.inputs(i).wrConfig.bits.wrdata_length
//         io.inputs(i).wrData.ready := false.B
//         /**************************
//         Inports of read connect
//         **************************/
//         io.inputs(i).rdConfig.ready:=rdarbiter.io.in(i).ready
//         rdarbiter.io.in(i).valid:=io.inputs(i).rdConfig.valid
//         rdarbiter.io.in(i).bits.rdAddr:=io.inputs(i).rdConfig.bits.rdAddr
//         rdarbiter.io.in(i).bits.rddata_length:=io.inputs(i).rdConfig.bits.rddata_length
//         io.inputs(i).rdData.valid := false.B
//         io.inputs(i).rdData.bits:=DontCare
//     }

//     when(wrarbiter.io.out.fire) {
//         wrconfigReg.addr:=wrarbiter.io.out.bits.wrAddr
//         wrconfigReg.data_length:=wrarbiter.io.out.bits.wrdata_length
//         wrconfigReg.ports_numlen:= OHToUInt(wrarbiter.io.chosen)
//     }

//     when(rdarbiter.io.out.fire) {
//         rdconfigReg.addr:=rdarbiter.io.out.bits.rdAddr
//         rdconfigReg.data_length:=rdarbiter.io.out.bits.rddata_length
//         rdconfigReg.ports_numlen:=OHToUInt(rdarbiter.io.chosen)
//     }    

//     wrarbiter.io.out.ready := wrconfigReg.data_length === 0.U
//     rdarbiter.io.out.ready:=Mux(rdconfigReg.data_length === 0.U, true.B, false.B)
    
//     io.output.wrEna:=io.inputs(wrconfigReg.ports_numlen).wrData.fire
//     io.inputs(wrconfigReg.ports_numlen).wrData.ready:= wrconfigReg.data_length =/= 0.U
    
//     when(io.inputs(wrconfigReg.ports_numlen).wrData.fire){
//         wrconfigReg.data_length:=wrconfigReg.data_length-1.U
//     }

//     io.inputs(rdconfigReg.ports_numlen).rdData.valid:=rdconfigReg.data_length =/= 0.U
//     when(io.inputs(rdconfigReg.ports_numlen).rdData.fire){
//         rdconfigReg.data_length:=rdconfigReg.data_length-1.U
//         io.inputs(rdconfigReg.ports_numlen).rdData.bits:=io.output.rdData
//     }
//     io.output.rdAddr:=rdconfigReg.addr
//     io.output.wrAddr:=wrconfigReg.addr
//     io.output.wrData:=io.inputs(wrconfigReg.ports_numlen).wrData.bits
// }