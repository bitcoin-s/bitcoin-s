package org.bitcoins.script.flag

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 4/6/16.
 */
class ScriptFlagUtilTest extends FlatSpec with MustMatchers {

  "ScriptFlagUtil" must "check if strict der encoding check is required" in {
    ScriptFlagUtil.requiresStrictDerEncoding(Seq(ScriptVerifyDerSig) ) must be (true)
    ScriptFlagUtil.requiresStrictDerEncoding(Seq(ScriptVerifyStrictEnc)) must be (true)
  }

  it must "return false if strict der encoding check is not required" in {
    ScriptFlagUtil.requiresStrictDerEncoding(Seq()) must be (false)

    ScriptFlagUtil.requiresStrictDerEncoding(Seq(ScriptVerifyCheckLocktimeVerify, ScriptVerifyCheckSequenceVerify,
      ScriptVerifyCleanStack, ScriptVerifyDiscourageUpgradableNOPs, ScriptVerifyLowS, ScriptVerifyMinimalData,
      ScriptVerifyNone, ScriptVerifyNullDummy, ScriptVerifyP2SH, ScriptVerifySigPushOnly)) must be (false)
  }
}
