package NetworkInterfaceArbiter
import chisel3._
import chisel3.util._
import mesh_network.Flit


class  NetworkInterfaceArbiterBundle(inPorts:Int) extends  Bundle{
    val inputs = Vec(inPorts, Flipped(DecoupledIO(new Flit)))
    val output = DecoupledIO(new Flit)
}

/*****************************************************************/

class RegBundle(inPorts:Int) extends Bundle{
    val temp_data= new Flit
    val selected_port = UInt(log2Ceil(inPorts).W)
}

class UserRegBundle(inPorts:Int) extends Bundle{
    val temp_data= new Flit
    val selected_port = UInt(log2Ceil(inPorts).W)
}



class priorityRegdataBundle extends Bundle{
    val vc_id = UInt(2.W)  // the most 4 vc channl
    val priority = UInt(2.W) //the most 4priority  
    val valid = Bool()
}

class prioritydataBundle extends Bundle{
    val vc_id = UInt(2.W)  // the most 4 vc channl
    val priority = UInt(2.W) //the most 4priority  
}

class  ModuleProrityBundle(inPorts:Int) extends  Bundle{
    val inputs = Vec(inPorts, Flipped(DecoupledIO(new prioritydataBundle)))
    val output = DecoupledIO(new prioritydataBundle)   
}
/***************************************************************************/

class  priorityReg extends Bundle{
    val tmp  = new Flit
    val valid = Bool()
}


class  outputReg extends Bundle{
    val selet = UInt(2.W)
    val valid = Bool()
}

class  fifocheckReg extends Bundle{
    val belong_VC = UInt(3.W)
    val fifobusy = Bool()
}