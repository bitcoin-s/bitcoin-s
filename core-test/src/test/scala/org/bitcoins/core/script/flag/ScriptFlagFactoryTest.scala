package org.bitcoins.core.script.flag

import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 3/23/16.
  */
class ScriptFlagFactoryTest extends BitcoinSUnitTest {

  "ScriptFlagFactory" must "find a NONE script flag" in {
    ScriptFlagFactory.fromString("NONE") must be(ScriptVerifyNone)
  }

  it must "find a P2SH script flag" in {
    ScriptFlagFactory.fromString("P2SH") must be(ScriptVerifyP2SH)
  }

  it must "find a STRICTENC flag" in {
    ScriptFlagFactory.fromString("STRICTENC") must be(ScriptVerifyStrictEnc)
  }

  it must "find a DERSIG flag" in {
    ScriptFlagFactory.fromString("DERSIG") must be(ScriptVerifyDerSig)
  }

  it must "find a LOW_S flag" in {
    ScriptFlagFactory.fromString("LOW_S") must be(ScriptVerifyLowS)
  }

  it must "find a SIGPUSHONLY flag" in {
    ScriptFlagFactory.fromString("SIGPUSHONLY") must be(ScriptVerifySigPushOnly)
  }

  it must "find a MINIMALDATA flag" in {
    ScriptFlagFactory.fromString("MINIMALDATA") must be(ScriptVerifyMinimalData)
  }

  it must "find a NULLDUMMY flag" in {
    ScriptFlagFactory.fromString("NULLDUMMY") must be(ScriptVerifyNullDummy)
  }

  it must "find a DISCOURAGE_UPGRADABLE_NOPS flag" in {
    ScriptFlagFactory.fromString("DISCOURAGE_UPGRADABLE_NOPS") must be(
      ScriptVerifyDiscourageUpgradableNOPs)
  }

  it must "find a CLEANSTACK flag" in {
    ScriptFlagFactory.fromString("CLEANSTACK") must be(ScriptVerifyCleanStack)
  }

  it must "find a CHECKLOCKTIMEVERIFY flag" in {
    ScriptFlagFactory.fromString("CHECKLOCKTIMEVERIFY") must be(
      ScriptVerifyCheckLocktimeVerify)

  }

  it must "find a CHECKSEQUENCEVERIFY flag" in {
    ScriptFlagFactory.fromString("CHECKSEQUENCEVERIFY") must be(
      ScriptVerifyCheckSequenceVerify)
  }

  it must "match a string version of a script flag" in {
    val str = "MINIMALDATA"
    ScriptFlagFactory.fromStringOpt(str) must be(Some(ScriptVerifyMinimalData))
  }

  it must "match a comma separated list of flags" in {
    val str = "P2SH,STRICTENC"
    ScriptFlagFactory.fromList(str) must be(
      Seq(ScriptVerifyP2SH, ScriptVerifyStrictEnc))
  }

  it must "match a sequence of strings with their script flags" in {
    val l = List("P2SH", "STRICTENC")
    ScriptFlagFactory.fromList(l) must be(
      Seq(ScriptVerifyP2SH, ScriptVerifyStrictEnc))
  }

}
