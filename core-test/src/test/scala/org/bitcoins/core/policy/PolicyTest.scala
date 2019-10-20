package org.bitcoins.core.policy

import org.bitcoins.core.script.flag._
import org.bitcoins.testkit.util.BitcoinSUnitTest

/**
  * Created by chris on 5/2/16.
  */
class PolicyTest extends BitcoinSUnitTest {

  "Policy" must "determine what the mandatory script verify flags are" in {
    Policy.mandatoryScriptVerifyFlags must be(Seq(ScriptVerifyP2SH))
  }

}
