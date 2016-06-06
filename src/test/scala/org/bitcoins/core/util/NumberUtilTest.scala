package org.bitcoins.core.util

import org.bitcoins.core.protocol.script.ScriptSignature
import org.bitcoins.core.protocol.CompactSizeUIntImpl
import org.bitcoins.core.script.constant.ScriptNumberUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/8/16.
 */
class NumberUtilTest extends FlatSpec with MustMatchers with NumberUtil {





  it must "parse a variable length integer (VarInt)" in {
    val str = "fdfd00"
    parseCompactSizeUInt(str) must be (CompactSizeUIntImpl(253,3))

    val str1 = "00"
    parseCompactSizeUInt(str1) must be (CompactSizeUIntImpl(0,1))

    val str2 = "ffffffffff"
    parseCompactSizeUInt(str2) must be (CompactSizeUIntImpl(4294967295L,9))
  }


  it must "parse a variable length integer the same from a tx input and a script sig" in {
    parseCompactSizeUInt(TestUtil.txInput.head.scriptSignature) must be (TestUtil.txInput.head.scriptSigCompactSizeUInt)
  }

  it must "parse multiple variable length integers correctly for a multi input tx" in {
    parseCompactSizeUInt(TestUtil.txInputs.head.scriptSignature) must be (TestUtil.txInputs.head.scriptSigCompactSizeUInt)
    parseCompactSizeUInt(TestUtil.txInputs(1).scriptSignature) must be (TestUtil.txInputs(1).scriptSigCompactSizeUInt)
  }

  it must "parse the variable length integer of the empty script" in {
    parseCompactSizeUInt(ScriptSignature.empty) must be (CompactSizeUIntImpl(0,1))
  }

}
