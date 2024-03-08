package operator

import chisel3._
import chisel3.util._
import mesh_network.NetworkConfig
import mesh_network.SystemConfig

class  Unifyfield extends  Bundle{
    val Address = UInt(4.W)
    val flag = UInt(2.W)//00:配置类型  01：秘钥类型  10：数据类型
}

class Unifypckage extends Bundle{
    val field = new Unifyfield
    val data = UInt(128.W)
}

class UnifyBundle extends Bundle{

    val DataIn = Flipped(Decoupled(new Unifypckage))
    val DataOut = Decoupled(new Unifypckage)
}


class  UnifyfieldReg extends  Bundle{
    val Address = UInt(4.W)
    val flag = UInt(2.W)//00:配置类型  01：秘钥类型  10：数据类型
    val data = UInt(128.W)
}
/******************************************************************************/
class AesSm4RegBundle extends Bundle{
    val data = UInt(128.W)
    val key= UInt(128.W)
    val cfg = UInt(128.W)
}
/*******************************AES-ECB******************************************/

class AesBundle extends Bundle{
    val text = ValidIO(UInt(128.W))
    val key= ValidIO(UInt(128.W))
    val cfg = UInt(128.W)
}


class SM4Bundle extends Bundle{
    val text = ValidIO(UInt(128.W))
    val key= ValidIO(UInt(128.W))
    val cfg = ValidIO(UInt(log2Ceil(SystemConfig.max_tasks).W))

    val tagpackagelenth =UInt(log2Ceil(NetworkConfig.tagpackagelenth_width).W)
    val currpackagelenth =UInt(log2Ceil(NetworkConfig.currpackagelength_width).W)
}

/*******************************SM3******************************************/
class SM3RegBundle extends  Bundle{
    val data = ValidIO(UInt(128.W))
}