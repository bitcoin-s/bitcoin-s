package org.bitcoins.protocol

import org.bitcoins.util.ScalacoinUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 7/26/15.
 */
class CommonStructuresTest extends FlatSpec with MustMatchers  {

  "CommonStructuresTest" must "parse a VarInt of length 1" in {

    NetworkVarInt("0").length must be (0)
    NetworkVarInt("00").length must be (0)
    NetworkVarInt("01").length must be (1)
    NetworkVarInt("FDFFFF").length must be (65535)
    NetworkVarInt("FEFFFFFFFF").length must be (4294967295L)
    NetworkVarInt("FFFFFFFFFFFFFFFFFF").length must be (scala.math.BigInt("18446744073709551615"))

  }

}
