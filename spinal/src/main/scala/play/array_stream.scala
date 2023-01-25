import spinal.core._
import spinal.lib._

case class AxiStreamBus(AXIS_DATA_WIDTH:Int, AXIS_KEEP_WIDTH:Int, AXIS_TX_USER_WIDTH:Int) extends Bundle with IMasterSlave {
  
  val tdata  = Bits(AXIS_DATA_WIDTH bits)
  val tkeep  = Bits(AXIS_KEEP_WIDTH bits)
  val tuser  = Bits(AXIS_TX_USER_WIDTH bits)

  // define the direction of the data in a master mode 
  override def asMaster(): Unit = {
    out(tdata, tkeep, tuser)
  }

}


case class AxiStreamBusVec(PORT_COUNT:Int, AXIS_DATA_WIDTH:Int, AXIS_KEEP_WIDTH:Int, AXIS_TX_USER_WIDTH:Int) extends Bundle with IMasterSlave {

  val tdata  = Bits(PORT_COUNT*AXIS_DATA_WIDTH bits)
  val tkeep  = Bits(PORT_COUNT*AXIS_KEEP_WIDTH bits)
  val tvalid = Bits(PORT_COUNT bits)
  val tready = Bits(PORT_COUNT bits)
  val tlast  = Bits(PORT_COUNT bits)
  val tuser  = Bits(PORT_COUNT*AXIS_TX_USER_WIDTH bits)

  // define the direction of the data in a master mode 
  override def asMaster(): Unit = {
    out(tdata, tkeep, tvalid, tlast, tuser)
    in(tready)
  }

  // Connect that to this
  def <<(that: Vec[Stream[Fragment[AxiStreamBus]]]): Unit = {
    for ((axis, index) <- that.zipWithIndex) {
      this.tdata((index+1)*AXIS_DATA_WIDTH-1 downto index*AXIS_DATA_WIDTH) := axis.tdata
      this.tkeep((index+1)*AXIS_KEEP_WIDTH-1 downto index*AXIS_KEEP_WIDTH) := axis.tkeep
      this.tvalid(index) := axis.valid
      axis.ready := this.tready(index)
      this.tlast(index) := axis.last
      this.tuser((index+1)*AXIS_TX_USER_WIDTH-1 downto index*AXIS_TX_USER_WIDTH) := axis.tuser
    }
  }

  // Connect this to that
  def >>(that: Vec[Stream[Fragment[AxiStreamBus]]]): Unit = {
    for ((axis, index) <- that.zipWithIndex) {
      axis.tdata := this.tdata((index+1)*AXIS_DATA_WIDTH-1 downto index*AXIS_DATA_WIDTH) 
      axis.tkeep := this.tkeep((index+1)*AXIS_KEEP_WIDTH-1 downto index*AXIS_KEEP_WIDTH) 
      axis.valid := this.tvalid(index) 
      this.tready(index) := axis.ready 
      axis.last := this.tlast(index) 
      axis.tuser := this.tuser((index+1)*AXIS_TX_USER_WIDTH-1 downto index*AXIS_TX_USER_WIDTH) 
    }
  }
}


//-------------------------------------------------------------------------------
// Class : ArrayStream
//-------------------------------------------------------------------------------
class ArrayStream(PORT_COUNT:Int, AXIS_DATA_WIDTH:Int, AXIS_KEEP_WIDTH:Int, AXIS_TX_USER_WIDTH:Int) extends Component {

  val io = new Bundle {
    val s_axis_direct_tx = slave (AxiStreamBusVec(PORT_COUNT, AXIS_DATA_WIDTH, AXIS_KEEP_WIDTH, AXIS_TX_USER_WIDTH))
  }
  noIoPrefix()

  val s_axis_direct_rx = Vec(Stream(Fragment(AxiStreamBus(AXIS_DATA_WIDTH, AXIS_KEEP_WIDTH, AXIS_TX_USER_WIDTH))), PORT_COUNT)
  for (axis <- s_axis_direct_rx) {
    axis.ready := True
  }
  io.s_axis_direct_tx >> s_axis_direct_rx

}

/*
object ArrayStream_Verilog {
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "netlist/verilog", oneFilePerComponent = true).generateVerilog(new ArrayStream(
      PORT_COUNT = 4,
      AXIS_DATA_WIDTH = 16,
      AXIS_KEEP_WIDTH = 2,
      AXIS_TX_USER_WIDTH = 8
    ))
  }
}
*/
