package tbps_crc
import spinal.core._
import spinal.lib._
// import utilities to run the formal verification, but also some utilities to describe formal stuff
import spinal.core.formal._


//-------------------------------------------------------------------------------
// Class : CrcByteEnWrapper
//-------------------------------------------------------------------------------
class CrcByteEnWrapper(cfg: CrcByteEnCfg) extends Component {
  import cfg._

  val io = new Bundle {
    val din         = in  UInt(DWIDTH bits)
    val byteEn      = in  UInt(DWIDTH/8 bits)
    val dlast       = in  Bool()
    val flitEn      = in  Bool()
    val dut_crc_out     = out (Reg(UInt(CRC_WIDTH bits)) init(0) addAttribute("keep", "true"))
    val dut_crc_out_vld = out (Reg(Bool()) init(False) addAttribute("keep", "true"))
    val model_crc_out     = out (Reg(UInt(CRC_WIDTH bits)) init(0) addAttribute("keep", "true"))
    val model_crc_out_vld = out (Reg(Bool()) init(False) addAttribute("keep", "true"))
  }
  noIoPrefix()

  val dut = new CrcByteEn(cfg)
  val model = new CrcByteEnBB(cfg)

  io.din             <> dut.io.din
  io.byteEn          <> dut.io.byteEn
  io.dlast           <> dut.io.dlast
  io.flitEn          <> dut.io.flitEn
  io.dut_crc_out     <> dut.io.crc_out
  io.dut_crc_out_vld <> dut.io.crc_out_vld

  io.din               <> model.io.din
  io.byteEn            <> model.io.byteEn
  io.dlast             <> model.io.dlast
  io.flitEn            <> model.io.flitEn
  io.model_crc_out     <> model.io.crc_out
  io.model_crc_out_vld <> model.io.crc_out_vld
}

//-------------------------------------------------------------------------------
// Class : CrcByteEnFormal
//-------------------------------------------------------------------------------
class CrcByteEnFormal(cfg: CrcByteEnCfg) extends Component {
    val dutWrapper = FormalDut(new CrcByteEnWrapper(cfg))

    assumeInitial(ClockDomain.current.isResetActive)

    anyseq(dutWrapper.io.din)
    anyseq(dutWrapper.io.byteEn)
    anyseq(dutWrapper.io.dlast)
    anyseq(dutWrapper.io.flitEn)
    assert(dutWrapper.io.dut_crc_out === dutWrapper.io.model_crc_out)
    assert(dutWrapper.io.dut_crc_out_vld === dutWrapper.io.model_crc_out_vld)
}

object CrcByteEnFormalVerify extends App {
  val cfg = CrcByteEnCfg(
    DWIDTH     = 512,
    CRC_WIDTH  = 32,
    PIPE_LVL   = 0,
    CRC_POLY   = 0X04C11DB7,
    INIT       = 0xffffffff,
    XOR_OUT    = 0xffffffff,
    REFIN      = true,
    REFOUT     = true
  )

  //FormalConfig.withBMC(15).doVerify(new CrcByteEnFormal(cfg))
  FormalConfig.withProve(15).doVerify(new CrcByteEnFormal(cfg))
}
