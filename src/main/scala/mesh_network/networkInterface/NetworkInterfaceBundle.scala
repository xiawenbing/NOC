package mesh_network
import chisel3._
import chisel3.util._
import scala.util.Random
import java.math.BigInteger

class  Unifyfield extends  Bundle{
    val Address = UInt(4.W)
    val flag = UInt(2.W)//00:配置类型  01：秘钥类型  10：数据类型
}

class Unifypckage extends Bundle{
    val field = new Unifyfield
    val data = UInt(128.W)
}

class InterfaceBundle extends Bundle{
    val NetworkInterfaceDataIN = Flipped(Decoupled(UInt(128.W)))
    val NetworkInterfaceDataOUT = Decoupled(UInt(128.W))
    val DataOut = Decoupled(new Unifypckage)  
    val DataIn = Flipped(Decoupled(new Unifypckage))

}

class configlenRegBundle extends Bundle{
    val data_len = UInt(4.W)
    val key_len= UInt(4.W)
    val cfg_len = UInt(4.W)
    val result_len = UInt(4.W)
}

class  UnifyfieldReg extends  Bundle{
    val Address = UInt(4.W)
    val flag = UInt(2.W)//00:配置类型  01：秘钥类型  10：数据类型
    val data = UInt(128.W)
}


/***************************************test_Bundle**********************************************/
class test_InterfaceBundle extends Bundle{
    val NetworkInterfaceDataIN = Flipped(Decoupled(new  Flit))
    val NetworkInterfaceDataOUT = Decoupled(new  Flit)

    val DataOut = Decoupled(new Unifypckage)  
    val DataIn = Flipped(Decoupled(new Unifypckage))

    val runCount = UInt(3.W)
}
/***************************************test_BundleCC**********************************************/
class test_InterfaceCCBundle extends Bundle{
    val NetworkInterfaceDataIN = Flipped(Decoupled(new  Flit))
    val NetworkInterfaceDataOUT = Decoupled(new  Flit)
    val DataOut = Decoupled(new Unifypckage)  
    val DataIn = Flipped(Decoupled(new Unifypckage))

    val NetworkInterfaceuserOUT = Decoupled(new PcbReg)
    val NetworkInterfaceuserIn = Flipped(Decoupled(UInt(log2Ceil(SystemConfig.max_users).W))) //输入用户ID   2位 00 01 10 11
    val runned = Output(Bool())

    val runCount = Output(UInt(3.W))
}

class test_InterfaceSM3CCBundle extends Bundle{
    val NetworkInterfaceDataIN = Flipped(Decoupled(new  Flit))
    val NetworkInterfaceDataOUT = Decoupled(new  Flit)
    val DataOut = Decoupled(new Unifypckage)  
    val DataIn = Flipped(Decoupled(new Unifypckage))

    val NetworkInterfaceuserOUT = Decoupled(new PcbReg)
    val NetworkInterfaceuserIn = Flipped(Decoupled(UInt(log2Ceil(SystemConfig.max_users).W))) //输入用户ID   2位 00 01 10 11
    val runned = Output(Bool())

    val runCount = Output(UInt(3.W))

    val tcp_Count = Output(UInt(3.W))
}


class test_InterfaceMULBundle extends Bundle{
    val NetworkInterfaceDataIN = Flipped(Decoupled(new  Flit))
    val NetworkInterfaceDataOUT = Decoupled(new  Flit)

    val NetworkInterfaceuserOUT = Decoupled(new PcbReg)
    val NetworkInterfaceuserIn = Flipped(Decoupled(UInt(log2Ceil(SystemConfig.max_users).W))) //输入用户ID   2位 00 01 10 11
    val runned = Output(Bool())

    val runCount = Output(UInt(3.W))
}





class CCBundle extends Bundle{
    val NetworkInterfaceuserIn = Flipped(Decoupled(new PcbReg))
    val NetworkInterfaceuserOUT = Decoupled(UInt(log2Ceil(SystemConfig.max_users).W))//输入用户ID 2位 00 01 10 11
    
    val runned = Input(Bool())
}


/***************************************************************************************************/

class test_Unifypckage extends Bundle{
    val field = new Unifyfield
    val data = new DataFlitLoad
}


class  test_UnifyfieldReg extends  Bundle{
    val ID = UInt(log2Ceil(SystemConfig.max_tasks).W)
    val Address = UInt(4.W)
    val flag = UInt(2.W)//00:配置类型  01：秘钥类型  10：数据类型
    val data = UInt(128.W)
}


class  NetworkInterfaceArbiterBundle_Top(inPorts:Int) extends  Bundle{
    val inputs = Vec(inPorts, Flipped(Decoupled(new Flit)))
    val output = Decoupled(new Flit)


    val runCount = Output(UInt(3.W))
}

class  sm3NetworkInterfaceArbiterBundle_Top(inPorts:Int) extends  Bundle{
    val inputs = Vec(inPorts, Flipped(Decoupled(new Flit)))
    val output = Decoupled(new Flit)

    val tcpCount = Output(UInt(3.W))
    val runCount = Output(UInt(3.W))
}

class  PcbReg extends Bundle{
    val valid = Bool()  //判断是否有效pcb
    val ID = UInt(log2Ceil(SystemConfig.max_users).W)
    val priority = UInt(log2Ceil(NetworkConfig.priority_width).W)
    val MemInfo = UInt(log2Ceil(SystemConfig.max_users).W) //八块用户data存储单元 每块 可存储 16x128bit 
                            // 实验时，mem索引地址根据ID号存放
    val tagpacklength = UInt(log2Ceil(NetworkConfig.tagpackagelenth_width).W)//实际数据包存放量最大16块
    val currpacklength = UInt(log2Ceil(NetworkConfig.currpackagelength_width).W)  //表示当前已接收的个数
    val finishpacklength = UInt(log2Ceil(NetworkConfig.finishpacklength_width).W)  //表示已经处理的Flit个数
    val config = UInt(128.W)
    val runned = Bool() //是否已经运行过


}

class SM4Pcb extends Bundle{
    val valid = Bool()  //判断是否有效pcb
    val ID = UInt(log2Ceil(SystemConfig.max_users).W)

    val tagpacklength = UInt(log2Ceil(NetworkConfig.tagpackagelenth_width).W)//实际数据包存放量最大16块
    val currpacklength =  UInt(log2Ceil(NetworkConfig.currpackagelength_width).W)
}