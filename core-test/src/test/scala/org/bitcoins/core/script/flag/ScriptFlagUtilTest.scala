package org.bitcoins.core.script.flag

import org.bitcoins.testkitcore.util.BitcoinSUnitTest

/** Created by chris on 4/6/16.
  */
class ScriptFlagUtilTest extends BitcoinSUnitTest {

  "ScriptFlagUtil" must "check if strict der encoding check is required" in {
    ScriptFlagUtil.requiresStrictDerEncoding(Seq(ScriptVerifyDerSig)) must be(
      true
    )
    ScriptFlagUtil.requiresStrictDerEncoding(
      Seq(ScriptVerifyStrictEnc)
    ) must be(true)
  }

  it must "return false if strict der encoding check is not required" in {
    ScriptFlagUtil.requiresStrictDerEncoding(Seq()) must be(false)

    // ScriptVerifyLowS is deliberately excluded here: Core's
    // CheckSignatureEncoding applies the DER check whenever DERSIG, LOW_S,
    // or STRICTENC is set, so LOW_S alone must make this return true (see
    // the dedicated LOW_S test below).
    ScriptFlagUtil.requiresStrictDerEncoding(
      Seq(
        ScriptVerifyCheckLocktimeVerify,
        ScriptVerifyCheckSequenceVerify,
        ScriptVerifyCleanStack,
        ScriptVerifyDiscourageUpgradableNOPs,
        ScriptVerifyMinimalData,
        ScriptVerifyNone,
        ScriptVerifyNullDummy,
        ScriptVerifyP2SH,
        ScriptVerifySigPushOnly
      )
    ) must be(false)
  }

  it must "check that strict der encoding is required when only LOW_S is set" in {
    ScriptFlagUtil.requiresStrictDerEncoding(Seq(ScriptVerifyLowS)) must be(
      true
    )
  }
}
