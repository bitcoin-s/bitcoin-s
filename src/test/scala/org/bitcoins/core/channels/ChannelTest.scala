package org.bitcoins.core.channels

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.P2SHScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionOutput}
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by chris on 5/31/17.
  */
class ChannelTest extends FlatSpec with MustMatchers {

  "ChannelTest" must "fail to create payment channel if we do not have the minimum amoutn required for initial deposit" in {
    val lock = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get._1
    val p2sh = P2SHScriptPubKey(lock)
    val amount = Policy.minPaymentChannelAmount - Satoshis.one
    val output = TransactionOutput(amount,p2sh)
    val tx = Transaction(TransactionConstants.version, Nil, Seq(output), TransactionConstants.lockTime)
    val aTx = AnchorTransaction(tx)
    val chan = PaymentChannelAwaitingAnchorTx(aTx,lock)
    chan.isFailure must be (true)
  }

  it must "fail to create a payment channel if one of the outputs scriptPubKey's is not P2SH(lock)" in {
    val lock = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get._1
    val randomScript = ScriptGenerators.scriptPubKey.sample.get._1
    val p2sh = P2SHScriptPubKey(randomScript)
    val amount = Policy.minPaymentChannelAmount
    val output = TransactionOutput(amount,p2sh)
    val tx = Transaction(TransactionConstants.version, Nil, Seq(output), TransactionConstants.lockTime)
    val aTx = AnchorTransaction(tx)
    val chan = PaymentChannelAwaitingAnchorTx(aTx,lock)
    chan.isFailure must be (true)
  }

  it must "fail to clientSign ChannelAwaitingAnchorTx if we do not have enough confs" in {
    val clientSPK = ScriptGenerators.scriptPubKey.sample.get._1
    val (lock,keys) = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get
    val p2sh = P2SHScriptPubKey(lock)
    val amount = Policy.minPaymentChannelAmount
    val output = TransactionOutput(amount,p2sh)
    val tx = Transaction(TransactionConstants.version, Nil, Seq(output), TransactionConstants.lockTime)
    val aTx = AnchorTransaction(tx)
    val chan = PaymentChannelAwaitingAnchorTx(aTx,lock)
    val clientSigned = chan.get.clientSign(clientSPK,amount,keys.head)
    clientSigned.isFailure must be (true)
  }

}
