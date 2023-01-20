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
  assert((DWIDTH % 8) == 0, s"DWIDTH $DWIDTH has to be a multiple of 8")
  assert(DWIDTH > 8, s"DWIDTH $DWIDTH is not larger than 1 byte. You don't really need this byte enable version")

  def gen_unified_table (): Vec[UInt] = {
    var table_old = Array.tabulate(CRC_WIDTH, DWIDTH+CRC_WIDTH)( (n,m) => false)
    var unified_table = Array.tabulate(CRC_WIDTH, DWIDTH+CRC_WIDTH)( (n,m) => false)
    val unified_table_hdl = Vec(UInt(DWIDTH+CRC_WIDTH bits), CRC_WIDTH)

    for (i <- 0 until CRC_WIDTH) {
      table_old(i)(i) = true
    }

    for (i <- 0 until 3) {
      printf (s"i = $i\n")
      for (i <- 0 until CRC_WIDTH) {
        for (j <- 0 until DWIDTH+CRC_WIDTH) {
          print (s"table_old($i)($j) = ")
          print (table_old(i)(j))
          print ("\n")
        }
      }

      unified_table(0) = table_old(CRC_WIDTH-1).clone
      unified_table(0)(CRC_WIDTH+DWIDTH-1-i) = !unified_table(0)(CRC_WIDTH+DWIDTH-1-i)

      for (j <- 1 until CRC_WIDTH) {
        if (((CRC_POLY >> j) & 0x1) == 0x1) {
          unified_table(j) = (table_old(j-1) zip unified_table(0)).map(t => t._1 ^ t._2)
        }
        else {
          unified_table(j) = table_old(j-1).clone
        }
      }
      table_old = unified_table.clone
    }

    for (i <- 0 until CRC_WIDTH) {
      for (j <- 0 until DWIDTH+CRC_WIDTH) {
        print (s"unified_table($i)($j) = ")
        print (unified_table(i)(j))
        print ("\n")
      }
    }

    for (i <- 0 until CRC_WIDTH) {
      for (j <- 0 until DWIDTH+CRC_WIDTH) {
        unified_table_hdl(i)(j) := Bool(unified_table(i)(j))
      }
    }
    return unified_table_hdl
  }

  val UNI_TABLE = gen_unified_table()

  val maskBytes = Vec(UInt(8 bits), DWIDTH/8)
  for (i <- 0 until DWIDTH/8) {
    maskBytes(i) := Mux(io.byteEn(i), U(0xff), U(0x00))
  }

  val mask_in_byte = Cat(maskBytes).asUInt

  val din_reg = Reg(cloneOf(io.din)) init(0)
  when (io.flitEn) {
    din_reg := io.din & mask_in_byte
  }

  val din_refin = UInt(DWIDTH bits)
  if (REFIN) {
    val din_reg_reversed = din_reg.reversed
    for (i <- 0 until DWIDTH/8) {
      din_refin((i+1) * 8 -1 downto i*8) := din_reg_reversed(DWIDTH - i*8 - 1 downto DWIDTH - (i+1) * 8)
    }
  }
  else {
    din_refin := din_reg
  }
  
  // TODO: crc_rev_en_pipe_wire -> $clog2 == log2Up

  val crc_rev_en_pipe_wire = UInt(log2Up(DWIDTH/8) bits)

  def pow2 (i: Int) = scala.math.pow(2,i).toInt

  for (i <- 0 until (log2Up(DWIDTH/8))) {
    var crc_rev_en_pipe_v:Bool = False

    for(j <- 0 until pow2(i)) {
      crc_rev_en_pipe_v = crc_rev_en_pipe_v | (~io.byteEn(DWIDTH/8-1-DWIDTH/8/pow2(i + 1)-j*(DWIDTH/8/pow2(i))) & io.byteEn(DWIDTH/8-1-j*(DWIDTH/8/pow2(i))))
    }

    crc_rev_en_pipe_wire(i) := crc_rev_en_pipe_v
  }

  val dlast_reg = Reg(UInt(PIPE_LVL+1 bits)) init(0)

  dlast_reg(0) := io.dlast
  for (i <- 1 to PIPE_LVL) {
    dlast_reg(i) := dlast_reg(i - 1)
  }

  val flitEn_reg = Reg(UInt(PIPE_LVL+1 bits)) init(0)

  flitEn_reg(0) := io.flitEn
  for (i <- 1 to PIPE_LVL) {
    flitEn_reg(i) := flitEn_reg(i - 1)
  }

  io.crc_out.assignDontCare() // TODO
  io.crc_out_vld.assignDontCare() // TODO
}

object CrcByteEn_Verilog {
  def main(args: Array[String]) {
    val cfg = CrcByteEnCfg(
      DWIDTH     = 16,//512,
      CRC_WIDTH  = 4,//32,
      PIPE_LVL   = 0,
      CRC_POLY   = 0X04C11DB7,
      INIT       = 0xffffffff,
      XOR_OUT    = 0xffffffff,
      REFIN      = true,
      REFOUT     = true
    )
    SpinalRtlConfig.generateVerilog(new CrcByteEn(cfg))
  }
}
