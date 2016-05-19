package org.bitcoins.core.policy

import org.bitcoins.core.script.flag._
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 5/2/16.
  */
class PolicyTest extends FlatSpec with MustMatchers {

  "Policy" must "determine what the mandatory script verify flags are" in {
    Policy.mandatoryScriptVerifyFlags must be (Seq(ScriptVerifyP2SH))
  }

  it must "determine what the non mandatory script verify flags are" in {
    Policy.standardNotMandatoryScriptVerifyFlags must be (Seq(ScriptVerifyDerSig, ScriptVerifyStrictEnc, ScriptVerifyMinimalData, ScriptVerifyNullDummy, ScriptVerifyDiscourageUpgradableNOPs, ScriptVerifyCleanStack, ScriptVerifyCheckLocktimeVerify, ScriptVerifyCheckSequenceVerify, ScriptVerifyLowS))
  }
}
