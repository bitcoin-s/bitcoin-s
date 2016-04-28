package org.bitcoins.crypto

import org.bitcoins.policy.Policy
import org.bitcoins.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 4/7/16.
 */
class TransactionSignatureComponentFactoryTest extends FlatSpec with MustMatchers {
  val component = TransactionSignatureComponentFactory.factory(TestUtil.transaction, 0, TestUtil.scriptPubKey, Policy.standardScriptVerifyFlags)

  "TransactionSignatureComponentFactory" must "create a tx signature component" in {
    component.transaction must be (TestUtil.transaction)
    component.inputIndex must be (0)
    component.scriptPubKey must be (TestUtil.scriptPubKey)
    component.flags must be (Policy.standardScriptVerifyFlags)

  }

  it must "replace a scriptPubKey with a new one" in {
    val newComponent = TransactionSignatureComponentFactory.factory(component,TestUtil.p2shScriptPubKey)
    newComponent.scriptPubKey must be (TestUtil.p2shScriptPubKey)
  }
}
