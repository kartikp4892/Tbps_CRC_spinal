package tbps_crc
import spinal.core._
import spinal.lib._

class CrcByteEnBB(cfg: CrcByteEnCfg) extends BlackBox {

  val generic = new Generic {
    val DWIDTH    = cfg.DWIDTH
    val CRC_WIDTH = cfg.CRC_WIDTH
    val PIPE_LVL  = cfg.PIPE_LVL
    val CRC_POLY  = cfg.CRC_POLY
    val INIT      = cfg.INIT
    val XOR_OUT   = cfg.XOR_OUT
    val REFIN     = cfg.REFIN
    val REFOUT    = cfg.REFOUT
  }

  val io = new Bundle {
    val clk = in Bool()
    val rst = in Bool()
    val din = in UInt(cfg.DWIDTH bits)
    val byteEn = in UInt(cfg.DWIDTH/8 bits)
    val dlast = in Bool()
    val flitEn = in Bool()
    val crc_out = out UInt(cfg.CRC_WIDTH bits)
    val crc_out_vld = out Bool()
  }
  noIoPrefix()
  setBlackBoxName("crc_gen_byteEn")

  mapClockDomain(clock=io.clk, reset=io.rst)
  addRTLPath("./core_src/crc_byteEn.sv")
}

