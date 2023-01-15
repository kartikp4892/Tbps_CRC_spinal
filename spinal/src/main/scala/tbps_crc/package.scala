import spinal.core._
import spinal.lib._

package object tbps_crc {

  object SpinalRtlConfig extends SpinalConfig(targetDirectory = "netlist/verilog", oneFilePerComponent = true) {
    override def generateVerilog[T <: Component](gen: => T): SpinalReport[T] = super.generateVerilog(gen)// .printPruned()
  }

}

