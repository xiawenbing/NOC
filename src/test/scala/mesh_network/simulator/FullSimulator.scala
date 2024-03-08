package mesh_network

import chisel3._
import chisel3.util._
import chiseltest._

// record the start and end of tasks.


object FullSimulator {
  type NetworkLayout = Map[(Int, Int), EndNodeDevice]
  type task_id = Int

  abstract class Task(val task_id: Int,
                      val dependencies: List[Int],
                      val source: (Int, Int),
                      val dest: (Int, Int)) {
    def toPacket(): soft_Packet
  }

  class ConfigRoute(task_id: Int,
                    dependencies: List[Int],
                    source: (Int, Int),
                    dest: (Int, Int),
                    val next_dest: (Int, Int))
    extends Task(task_id, dependencies, source, dest) {
    override def toPacket(): soft_Packet = new soft_RouteConfigPacket(source, dest, task_id, next_dest)
  }

  class MemCtrlrCall(task_id: Int,
                    dependencies: List[Int],
                    source: (Int, Int),
                    dest: (Int, Int),
                    val r_or_w: Boolean,
                    val length: Int)
    extends Task(task_id, dependencies, source, dest) {
    override def toPacket(): soft_Packet = new soft_MemCtrlrPacket(source, dest, task_id, r_or_w, length)
  }

  class SimulationPara(val layout: NetworkLayout,
                       val tasks: List[Task])

  def simulate(p: SimulationPara)(implicit d: NetworkExample) = {
    import NetworkConfig._
    import helper._
    require(p.layout.size == nodes)
    def findMaster: RouterPort = {
      val find = p.layout.filter{case(_, device) => device match {
        case _: MasterDevice => true
        case _ => false
      }}
      require(find.size == 1)
      d.io.locals(coordinate2Idx(find.head._1))
    }
    val master_device_port = findMaster
    var pending_tasks: List[Task] = p.tasks
    var finished_tasks: List[Task] = Nil
    var injection_buffer: List[soft_Flit] = Nil
    var injection_vc: Int = 0
    
    def readyToExecute(task: Task): Boolean = 
      task.dependencies.map{
        case n => finished_tasks.exists(_.task_id == n)
      }.reduce(_ && _)

    while(finished_tasks.length != p.tasks.length) {
      if(!pending_tasks.isEmpty && injection_buffer.isEmpty) {
        // try to handle a new task
        val new_task = pending_tasks.head
        if(readyToExecute(new_task) && !freeVCs(master_device_port).isEmpty) {
          // create a packet for this task and place it into injection_buffer
          val flits = new_task.toPacket().toFlits
          injection_buffer = flits.tail
          injection_vc = pickVC(freeVCs(master_device_port))
          // poke the first flit 
          pokeFlit(master_device_port.flit_in, flits.head, injection_vc)
          master_device_port.flit_in.valid.poke(true)
        }
      } else if(!injection_buffer.isEmpty){
        // keep injecting former packet
        if(master_device_port.credit_out(injection_vc).peekInt() > 1) {
          pokeFlit(master_device_port.flit_in, injection_buffer.head, injection_vc)
          master_device_port.flit_in.valid.poke(true)
          injection_buffer = injection_buffer.tail
        } else {
          master_device_port.flit_in.valid.poke(false)
        }
      } else { // no more pending tasks, injection buffer is empty
        master_device_port.flit_in.valid.poke(false)
      }

      // call the tick method on each EndNodeDevice
      p.layout.foreach{case (position, device) =>
        val idx = coordinate2Idx(position)
        val port = d.io.locals(idx)
        device.tick(port)
      }

      // step forward
      d.clock.step()
    }
  }
}