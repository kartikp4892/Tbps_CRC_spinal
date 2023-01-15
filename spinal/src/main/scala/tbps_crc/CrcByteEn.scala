package tbps_crc

import spinal.core._
import spinal.lib._

class CrcByteEn (cfg: CrcByteEnCfg) extends Component {
  import cfg._

  val io = new Bundle {
    val din         = in  UInt(DWIDTH bits)
    val byteEn      = in  UInt(DWIDTH/8 bits)
    val dlast       = in  Bool()
    val flitEn      = in  Bool()
    val crc_out     = out (Reg(UInt(CRC_WIDTH bits)) init(0) addAttribute("keep", "true"))
    val crc_out_vld = out (Reg(Bool()) init(False) addAttribute("keep", "true"))
  }
  noIoPrefix()

  io.crc_out.assignDontCare() // TODO
  io.crc_out_vld.assignDontCare() // TODO
}

object CrcByteEn_Verilog {
  def main(args: Array[String]) {
    val cfg = CrcByteEnCfg(
      DWIDTH     = 512,
      CRC_WIDTH  = 16,
      PIPE_LVL   = 0,
      CRC_POLY   = 0xda5f,
      INIT       = 0,
      XOR_OUT    = 0,
      REFIN      = false,
      REFOUT     = false
    )
    SpinalRtlConfig.generateVerilog(new CrcByteEn(cfg))
  }
}
