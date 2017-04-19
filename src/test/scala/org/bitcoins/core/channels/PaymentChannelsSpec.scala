package org.bitcoins.core.channels

import org.bitcoins.core.crypto.TxSigComponent
import org.bitcoins.core.gen.ChannelGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.scalacheck.{Prop, Properties}

/**
  * Created by chris on 4/18/17.
  */
class PaymentChannelsSpec extends Properties("PaymentChannelProperties") {

/*  property("spend a anchor transaction with the first spendingTx in a payment channel") =
    Prop.forAll(ChannelGenerators.freshPaymentChannelInProgress) { case (inProgress,_,_) =>
        val txSigComponent = TxSigComponent(inProgress.currentSpendingTx,UInt32.zero,
          inProgress.lock,Policy.standardScriptVerifyFlags)
        val p = ScriptProgram(txSigComponent)
        val result = ScriptInterpreter.run(p)
        result == ScriptOk
    }*/

}
