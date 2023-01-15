package tbps_crc

import spinal.core._
import spinal.lib._

//-------------------------------------------------------------------------------
// Class : CrcByteEnCfg
//-------------------------------------------------------------------------------
case class CrcByteEnCfg(
    var DWIDTH     :Int     = 512,
    var CRC_WIDTH  :Int     = 16,
    var PIPE_LVL   :Int     = 0,
    var CRC_POLY   :Int     = 0xda5f,
    var INIT       :Int     = 0,
    var XOR_OUT    :Int     = 0,
    var REFIN      :Boolean = false,
    var REFOUT     :Boolean = false
);

