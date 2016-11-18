package org.bitcoins.core.crypto

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.SigVersionBase
import org.bitcoins.core.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 4/7/16.
 */
class TransactionSignatureComponentTest extends FlatSpec with MustMatchers {
  val component = TransactionSignatureComponent(TestUtil.transaction, UInt32.zero, TestUtil.scriptPubKey,
    Policy.standardScriptVerifyFlags)

  "TransactionSignatureComponentFactory" must "create a tx signature component" in {
    component.transaction must be (TestUtil.transaction)
    component.inputIndex must be (UInt32.zero)
    component.scriptPubKey must be (TestUtil.scriptPubKey)
    component.flags must be (Policy.standardScriptVerifyFlags)
  }

  it must "replace a scriptPubKey with a new one" in {
    val newComponent = TransactionSignatureComponent(component,TestUtil.p2shScriptPubKey)
    newComponent.scriptPubKey must be (TestUtil.p2shScriptPubKey)
  }
}
