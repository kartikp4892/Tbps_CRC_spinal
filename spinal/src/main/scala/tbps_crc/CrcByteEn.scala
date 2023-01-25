package tbps_crc

import spinal.core._
import spinal.lib._
import scala.util.control.Breaks._
import scala.collection.mutable.{ArrayBuffer, Stack}

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

  def pow2 (i: Int) = scala.math.pow(2,i).toInt

  def gen_unified_table (): Array[Array[Boolean]] = {
    var table_old = Array.tabulate(CRC_WIDTH, DWIDTH+CRC_WIDTH)( (n,m) => false)
    var unified_table = Array.tabulate(CRC_WIDTH, DWIDTH+CRC_WIDTH)( (n,m) => false)

    for (i <- 0 until CRC_WIDTH) {
      table_old(i)(i) = true
    }

    for (i <- 0 until 3) {
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

    return unified_table
  }

  def gen_crc_table (unified_table: Array[Array[Boolean]]): Array[Array[Boolean]] = {

    var crc_table = Array.tabulate(CRC_WIDTH, CRC_WIDTH)((n,m) => false)

    for(i <- 0 until CRC_WIDTH){
      for (j <- 0 until CRC_WIDTH){
        crc_table(i)(j) = unified_table(i)(j)
      }
    }
    return crc_table
  }

  def gen_data_table (unified_table: Array[Array[Boolean]]): Array[Array[Boolean]] = {
    val data_table = Array.tabulate(CRC_WIDTH, DWIDTH)((n,m) => false)

    for(i <- 0 until CRC_WIDTH) {
      for (j <- 0 until DWIDTH){
        data_table(i)(j) = unified_table(i)(j + CRC_WIDTH)
      }
    }
    return data_table
  }

  def get_div_per_lvl ():Int = {
    var divider_per_lvl:Int = 0
    var n_last_lvl:Int = 0
    var j:Int = 0
    if (PIPE_LVL == 0) {
      divider_per_lvl = DWIDTH
    }
    else {
      var odone:Boolean = false
      j = 0
      n_last_lvl = 1
      while (!odone) {
        var idone:Boolean = false
        while (!idone) {
          if (n_last_lvl*(scala.math.pow(j,PIPE_LVL).toInt) >= DWIDTH) {
            idone = true
          }
          else {
            j += 1
          }
        }
        if (n_last_lvl+CRC_WIDTH >= j) {
          odone = true
        }
        else {
          n_last_lvl += 1
          j = 0
        }
      }
      divider_per_lvl = j
    }

    return divider_per_lvl
  }

  def get_n_terms (divider_per_lvl: Int):Array[Int] = {
    val n_terms = Array.fill(PIPE_LVL+1)(0)
    n_terms(0) = DWIDTH
    for (i <- 1 to PIPE_LVL) {
      n_terms(i) = (n_terms(i-1)-1)/divider_per_lvl+1
    }

    return n_terms
  }

  def get_branch_enable_table (
    data_table: Array[Array[Boolean]],
    divider_per_lvl: Int,
    n_terms: Array[Int]
  ) : Array[Array[Array[Boolean]]] = {

    var branch_enable_table = Array.tabulate(PIPE_LVL+1, CRC_WIDTH, (DWIDTH-1)/DIV_PER_LVL + 1)((i,j,k) => false)
    var n_terms_int:Int = 0
    if (PIPE_LVL != 0) {
      n_terms_int = n_terms(0)
      for (i <- 0 until CRC_WIDTH) {
        for (j <- 0 until (n_terms_int-1)/divider_per_lvl) {
          var k = j*divider_per_lvl
          breakable {
            while (k < (j+1)*divider_per_lvl && k < n_terms_int) {
              if (data_table(i)(k)) {
                branch_enable_table(0)(i)(j) = true
                break
              }
              k += 1
            }
          }
        }
      }
      for (i <- 1 until PIPE_LVL) {
        n_terms_int = n_terms(i)
        for (j <- 0 until CRC_WIDTH) {
          for (k <- 0 to (n_terms_int-1)/divider_per_lvl) {
            var m:Int = k*divider_per_lvl
            breakable {
              while (m < (k+1)*divider_per_lvl && m < n_terms_int) {
                if (branch_enable_table(i-1)(j)(m)) {
                  branch_enable_table(i)(j)(k) = true
                  break
                }
                m += 1
              }
            }
          }
        }
      }
    }
    return branch_enable_table;
  }

  def get_revert_table (): Array[Array[Array[Boolean]]] = {
    var table_old = Array.tabulate(CRC_WIDTH, CRC_WIDTH)((n,m) => false)
    var revert_table = Array.tabulate(log2Up(DWIDTH/8), CRC_WIDTH, CRC_WIDTH)((i,j,k) => false)
    for (i <- 0 until log2Up(DWIDTH/8)) {
      table_old = Array.tabulate(CRC_WIDTH, CRC_WIDTH)((n,m) => false)
      for (j <- 0 until CRC_WIDTH) {
        table_old(j)(j) = true
      }
      for (j <- 0 until (DWIDTH/(pow2(i+1)))) {
        revert_table(i)(CRC_WIDTH-1) = table_old(0).clone
        for (k <- 0 until CRC_WIDTH-1) {
          if (((CRC_POLY >> (k+1)) & 0x1) == 0x1) {
            revert_table(i)(k) = (table_old(k+1) zip table_old(0)).map(t => t._1 ^ t._2)
          }
          else {
            revert_table(i)(k) = table_old(k+1).clone
          }
        }
        table_old = revert_table(i).clone
      }
    }
    return revert_table
  }

  val DIV_PER_LVL = get_div_per_lvl
  val UNI_TABLE = gen_unified_table()
  val CRC_TABLE = gen_crc_table(UNI_TABLE)
  val DATA_TABLE = gen_data_table(UNI_TABLE)
  val N_TERMS = get_n_terms(DIV_PER_LVL)
  val BRANCH_ENABLE_TABLE = get_branch_enable_table(DATA_TABLE, DIV_PER_LVL, N_TERMS)
  val REVERT_TABLE = get_revert_table()

  val DEBUG_DIV_PER_LVL         = U(DIV_PER_LVL        )
  val DEBUG_UNI_TABLE           = Vec(UInt(DWIDTH+CRC_WIDTH bits), CRC_WIDTH)
  DEBUG_UNI_TABLE.zipWithIndex.foreach{
    case (r,i) =>
      for (j <- 0 until r.getWidth) {
        DEBUG_UNI_TABLE(i)(j).assignFrom(Bool(UNI_TABLE(i)(j)))
      }
  }
  val DEBUG_CRC_TABLE           = Vec(UInt(CRC_WIDTH bits), CRC_WIDTH)
  DEBUG_CRC_TABLE.zipWithIndex.foreach{
    case (r,i) =>
      for (j <- 0 until r.getWidth) {
        DEBUG_CRC_TABLE(i)(j).assignFrom(Bool(CRC_TABLE(i)(j)))
      }
  }
  val DEBUG_DATA_TABLE          = Vec(UInt(DWIDTH bits), CRC_WIDTH)
  DEBUG_DATA_TABLE.zipWithIndex.foreach{
    case (r,i) =>
      for (j <- 0 until r.getWidth) {
        DEBUG_DATA_TABLE(i)(j).assignFrom(Bool(DATA_TABLE(i)(j)))
      }
  }
  val DEBUG_N_TERMS             = Vec(UInt(31 bits), PIPE_LVL+1)
  DEBUG_N_TERMS.zipWithIndex.foreach{
    case (r,i) =>
      DEBUG_N_TERMS(i).assignFrom(U(N_TERMS(i)))
  }
  
  val DEBUG_BRANCH_ENABLE_TABLE = Vec(Vec(UInt((DWIDTH-1)/DIV_PER_LVL+1 bits), CRC_WIDTH), PIPE_LVL+1)
  DEBUG_BRANCH_ENABLE_TABLE.zipWithIndex.foreach{
    case (r,i) =>
      r.zipWithIndex.foreach{
        case (r2, j) =>
          for (k <- 0 until r2.getWidth) {
            DEBUG_BRANCH_ENABLE_TABLE(i)(j)(k).assignFrom(Bool(BRANCH_ENABLE_TABLE(i)(j)(k)))
          }
      }
  }

  val DEBUG_REVERT_TABLE        = Vec(Vec(UInt(CRC_WIDTH bits), CRC_WIDTH), log2Up(DWIDTH/8))
  DEBUG_REVERT_TABLE.zipWithIndex.foreach{
    case (r,i) =>
      r.zipWithIndex.foreach{
        case (r2, j) =>
          for (k <- 0 until r2.getWidth) {
            DEBUG_REVERT_TABLE(i)(j)(k).assignFrom(Bool(REVERT_TABLE(i)(j)(k)))
          }
      }
  }

  val maskBytes = Vec(UInt(8 bits), DWIDTH/8)
  val dlast_reg = History(RegNext(io.dlast, init=False), PIPE_LVL+1, init=False)
  val flitEn_reg = History(RegNext(io.flitEn, init=False), PIPE_LVL+1, init=False)
  
  val crc_int = UInt(CRC_WIDTH bits)
  var crc_int_v = Array.tabulate(CRC_WIDTH)(n => False)
  val crc_previous = Reg(UInt(CRC_WIDTH bits)) init(S(INIT, CRC_WIDTH bits).asUInt) addAttribute("keep", "true")

  var crc_refout_v = UInt(CRC_WIDTH bits)
  val crc_refout = UInt(CRC_WIDTH bits)
  val crc_rev_wire = Vec(UInt(CRC_WIDTH bits), log2Up(DWIDTH/8))
  var crc_rev_wire_v = Array.tabulate(log2Up(DWIDTH/8), CRC_WIDTH)((n,m) => False)
  val crc_rev_reg = Vec(Reg(UInt(CRC_WIDTH bits)) init(0), log2Up(DWIDTH/8))
  val crc_rev_en_pipe_wire = UInt(log2Up(DWIDTH/8) bits)
  val crc_rev_en_pipe_reg = History(RegNext(crc_rev_en_pipe_wire, init=U(0)), PIPE_LVL+1, init=U(0, log2Up(DWIDTH/8) bits))
  val crc_rev_en_reg = History(RegNext(crc_rev_en_pipe_reg(PIPE_LVL), init=U(0, log2Up(DWIDTH/8) bits)), log2Up(DWIDTH/8) , init=U(0, log2Up(DWIDTH/8) bits))

  var data_pipe = Vec(Vec(UInt((DWIDTH-1)/DIV_PER_LVL+1 bits), CRC_WIDTH), PIPE_LVL+1)
  var data_pipe_v = Array.tabulate(PIPE_LVL+1, CRC_WIDTH, (DWIDTH-1)/DIV_PER_LVL+1)((i,j,k) => False)
  val data_pipe_reg = Vec(Vec(Reg(UInt((DWIDTH-1)/DIV_PER_LVL+1 bits)) init(0), CRC_WIDTH), PIPE_LVL+1)
  val crc_vld_rev_reg = History(RegNext(flitEn_reg(PIPE_LVL) & dlast_reg(PIPE_LVL), init=False), log2Up(DWIDTH/8), init=False)

  val mask_in_byte = Cat(maskBytes).asUInt
  val din_reg = Reg(cloneOf(io.din)) init(0)

  for (i <- 0 until DWIDTH/8) {
    maskBytes(i) := Mux(io.byteEn(i), U(0xff), U(0x00))
  }

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


  for (i <- 0 until (log2Up(DWIDTH/8))) {
    var crc_rev_en_pipe_v:Bool = False

    for(j <- 0 until pow2(i)) {
      crc_rev_en_pipe_v = crc_rev_en_pipe_v | (~io.byteEn(DWIDTH/8-1-DWIDTH/8/pow2(i + 1)-j*(DWIDTH/8/pow2(i))) & io.byteEn(DWIDTH/8-1-j*(DWIDTH/8/pow2(i))))
    }

    crc_rev_en_pipe_wire(i) := crc_rev_en_pipe_v
  }

  //generate the first level
  for {
    i <- 0 until CRC_WIDTH
    j <- 0 until (DWIDTH-1)/DIV_PER_LVL+1
  } {
    var k:Int = 0
    while (k < DIV_PER_LVL && j*DIV_PER_LVL+k < DWIDTH) {
      if (DATA_TABLE(i)(j*DIV_PER_LVL+k)) {
        data_pipe_v(0)(i)(j) \= data_pipe_v(0)(i)(j) ^ din_refin(j*DIV_PER_LVL+k)
      }
      k += 1
    }
  }

  //level 2 -> aggregate data chain into 1 bit per crc bit
  for {
    i <- 1 to PIPE_LVL
    j <- 0 until CRC_WIDTH
    k <- 0 until (N_TERMS(i)-1)/DIV_PER_LVL+1
  } {
    var m:Int = k*DIV_PER_LVL
    while (m < (k+1)*DIV_PER_LVL && m < N_TERMS(i)) {
      if (BRANCH_ENABLE_TABLE(i-1)(j)(m)) {
        data_pipe_v(i)(j)(k) \= data_pipe_v(i)(j)(k) ^ data_pipe_reg(i-1)(j)(m);
      }
      m += 1
    }
  }

  data_pipe.zipWithIndex.foreach{
    case (_, i) =>
      data_pipe(i).zipWithIndex.foreach{
        case (_, j) =>
          data_pipe(i)(j) := Vec(data_pipe_v(i)(j)).asBits.asUInt
      }
  }

  //the last level
  for (i <- 0 until CRC_WIDTH) {
    for (j <- 0 until CRC_WIDTH) {
      if (CRC_TABLE(i)(j)) {
        crc_int_v(i) \= crc_int_v(i) ^ crc_previous(j)
      }
    }
    crc_int_v(i) \= crc_int_v(i) ^ data_pipe(PIPE_LVL)(i)(0)
  }
  crc_int := Vec(crc_int_v).asBits.asUInt

  for(i <- 0 until (log2Up(DWIDTH/8))) {
    for(j <- 0 until CRC_WIDTH) {
      for(k <- 0 until CRC_WIDTH) {
        if (REVERT_TABLE(i)(j)(k)) {
          crc_rev_wire_v(i)(j) \= crc_rev_wire_v(i)(j) ^ crc_rev_reg(i)(k)
        }
      }
    }
  }

  crc_rev_wire.zipWithIndex.foreach {
    case (e, i) =>
      e.assignFromBits( Vec(crc_rev_wire_v(i)).asBits )
  }

  crc_rev_reg(0) := crc_int

  for(i <- 1 until (log2Up(DWIDTH/8))) {
    when (crc_rev_en_reg(i - 1)(i - 1)) {
      crc_rev_reg(i) := crc_rev_wire(i - 1)
    }
    .otherwise {
      crc_rev_reg(i) := crc_rev_reg(i - 1)
    }
  }

  when(crc_rev_en_reg(log2Up(DWIDTH/8) - 1)(log2Up(DWIDTH/8) - 1)){
    crc_refout_v := crc_rev_wire(log2Up(DWIDTH/8) - 1)
  }
  .otherwise {
    crc_refout_v := crc_rev_reg(log2Up(DWIDTH/8) - 1)
  }

  if (REFOUT) {
    crc_refout_v \= crc_refout_v.reversed
  }

  crc_refout := crc_refout_v

  when (flitEn_reg(PIPE_LVL) && dlast_reg(PIPE_LVL)) {
    crc_previous := S(INIT, CRC_WIDTH bits).asUInt
  }
  .elsewhen (flitEn_reg(PIPE_LVL)) {
    crc_previous := crc_int
  }

  when (crc_vld_rev_reg(log2Up(DWIDTH/8)-1)) {
    io.crc_out := crc_refout ^ S(XOR_OUT, CRC_WIDTH bits).asUInt
  }
  io.crc_out_vld := crc_vld_rev_reg(log2Up(DWIDTH/8)-1)
}

object CrcByteEn_Verilog {
  def main(args: Array[String]) {
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
    SpinalRtlConfig.generateVerilog(new CrcByteEn(cfg))
  }
}
