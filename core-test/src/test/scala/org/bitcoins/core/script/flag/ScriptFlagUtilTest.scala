package org.bitcoins.core.script.flag

import org.bitcoins.testkit.util.BitcoinSUnitTest

/** Created by chris on 4/6/16.
  */
class ScriptFlagUtilTest extends BitcoinSUnitTest {

  "ScriptFlagUtil" must "check if strict der encoding check is required" in {
    ScriptFlagUtil.requiresStrictDerEncoding(Seq(ScriptVerifyDerSig)) must be(
      true)
    ScriptFlagUtil.requiresStrictDerEncoding(
      Seq(ScriptVerifyStrictEnc)) must be(true)
  }

  it must "return false if strict der encoding check is not required" in {
    ScriptFlagUtil.requiresStrictDerEncoding(Seq()) must be(false)

    ScriptFlagUtil.requiresStrictDerEncoding(
      Seq(
        ScriptVerifyCheckLocktimeVerify,
        ScriptVerifyCheckSequenceVerify,
        ScriptVerifyCleanStack,
        ScriptVerifyDiscourageUpgradableNOPs,
        ScriptVerifyLowS,
        ScriptVerifyMinimalData,
        ScriptVerifyNone,
        ScriptVerifyNullDummy,
        ScriptVerifyP2SH,
        ScriptVerifySigPushOnly
      )) must be(false)
  }
}
