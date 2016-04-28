package org.bitcoins.script.flag

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 3/23/16.
 */
class ScriptFlagFactoryTest extends FlatSpec with MustMatchers {

  "ScriptFlagFactory" must "find a NONE script flag" in {
    ScriptFlagFactory.fromString("NONE").get must be (ScriptVerifyNone)
  }

  it must "find a P2SH script flag" in {
    ScriptFlagFactory.fromString("P2SH").get must be (ScriptVerifyP2SH)
  }

  it must "find a STRICTENC flag" in {
    ScriptFlagFactory.fromString("STRICTENC").get must be (ScriptVerifyStrictEnc)
  }

  it must "find a DERSIG flag" in {
    ScriptFlagFactory.fromString("DERSIG").get must be (ScriptVerifyDerSig)
  }

  it must "find a LOW_S flag" in {
    ScriptFlagFactory.fromString("LOW_S").get must be (ScriptVerifyLowS)
  }

  it must "find a SIGPUSHONLY flag" in {
    ScriptFlagFactory.fromString("SIGPUSHONLY").get must be (ScriptVerifySigPushOnly)
  }

  it must "find a MINIMALDATA flag" in {
    ScriptFlagFactory.fromString("MINIMALDATA").get must be (ScriptVerifyMinimalData)
  }

  it must "find a NULLDUMMY flag" in {
    ScriptFlagFactory.fromString("NULLDUMMY").get must be (ScriptVerifyNullDummy)
  }

  it must "find a DISCOURAGE_UPGRADABLE_NOPS flag" in {
    ScriptFlagFactory.fromString("DISCOURAGE_UPGRADABLE_NOPS").get must be (ScriptVerifyDiscourageUpgradableNOPs)
  }

  it must "find a CLEANSTACK flag" in {
    ScriptFlagFactory.fromString("CLEANSTACK").get must be (ScriptVerifyCleanStack)
  }

  it must "find a CHECKLOCKTIMEVERIFY flag" in {
    ScriptFlagFactory.fromString("CHECKLOCKTIMEVERIFY").get must be (ScriptVerifyCheckLocktimeVerify)

  }

  it must "find a CHECKSEQUENCEVERIFY flag" in {
    ScriptFlagFactory.fromString("CHECKSEQUENCEVERIFY").get must be (ScriptVerifyCheckSequenceVerify)
  }

  it must "match a string version of a script flag" in {
    val str = "MINIMALDATA"
    ScriptFlagFactory.fromString(str) must be (Some(ScriptVerifyMinimalData))
  }

  it must "match a comma separated list of flags" in {
    val str = "P2SH,STRICTENC"
    ScriptFlagFactory.fromList(str) must be (Seq(ScriptVerifyP2SH, ScriptVerifyStrictEnc))
  }

  it must "match a sequence of strings with their script flags" in {
    val l = List("P2SH", "STRICTENC")
    ScriptFlagFactory.fromList(l) must be (Seq(ScriptVerifyP2SH, ScriptVerifyStrictEnc))
  }

}
