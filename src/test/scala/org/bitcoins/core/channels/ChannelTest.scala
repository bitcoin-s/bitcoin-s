package org.bitcoins.core.channels

import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.gen.ScriptGenerators
import org.bitcoins.core.number.Int64
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{P2SHScriptPubKey, P2WSHWitnessSPKV0, WitnessScriptPubKeyV0}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.BitcoinSLogger
import org.scalatest.{FlatSpec, MustMatchers}

import scala.util.Try

/**
  * Created by chris on 5/31/17.
  */
class ChannelTest extends FlatSpec with MustMatchers {
  private def logger = BitcoinSLogger.logger

  "ChannelTest" must "fail to create payment channel if we do not have the minimum amoutn required for initial deposit" in {
    val lock = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get._1
    val p2sh = P2SHScriptPubKey(lock)
    val amount = Policy.minChannelAmount - Satoshis.one
    val output = TransactionOutput(amount, p2sh)
    val tx = BaseTransaction(TransactionConstants.version, Nil, Seq(output), TransactionConstants.lockTime)
    val chan = ChannelAwaitingAnchorTx(tx, lock)
    chan.isFailure must be(true)
  }

  it must "fail to create a payment channel if one of the outputs scriptPubKey's is not P2SH(lock)" in {
    val lock = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get._1
    val randomScript = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val p2sh = P2SHScriptPubKey(randomScript)
    val amount = Policy.minChannelAmount
    val output = TransactionOutput(amount, p2sh)
    val tx = BaseTransaction(TransactionConstants.version, Nil, Seq(output), TransactionConstants.lockTime)
    val chan = ChannelAwaitingAnchorTx(tx, lock)
    chan.isFailure must be(true)

    //it must also fail if we do not have a p2sh output at all
    val output2 = TransactionOutput(amount, randomScript)
    val tx2 = BaseTransaction(TransactionConstants.version, Nil, Seq(output2), TransactionConstants.lockTime)
    val chan2 = ChannelAwaitingAnchorTx(tx2, lock)
    chan2.isFailure must be(true)
  }

  it must "fail to client sign ChannelAwaitingAnchorTx if we do not have enough confs" in {
    val clientSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val (lock, keys) = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get
    val p2sh = P2SHScriptPubKey(P2WSHWitnessSPKV0(lock))
    val amount = Policy.minChannelAmount
    val output = TransactionOutput(amount, p2sh)
    val tc = TransactionConstants
    val tx = BaseTransaction(tc.version, Nil, Seq(output), tc.lockTime)
    val chan = ChannelAwaitingAnchorTx(tx, lock)
    val clientSigned = chan.get.clientSign(clientSPK, amount, keys.head)
    clientSigned.isFailure must be(true)
  }

  it must "fail to client sign a payment channel in progress if the value is more than the locked amount" in {
    val (inProgress,keys) = validInProgress
    val i = inProgress.clientSign(inProgress.lockedAmount, keys.head)
    i.isFailure must be (true)
  }

  it must "be valid for the server to receive all of the money when a payment channel closes" in {
    val clientSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val serverSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val (lock,keys) = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get
    val p2sh = P2SHScriptPubKey(P2WSHWitnessSPKV0(lock))
    val amount = CurrencyUnits.oneBTC
    val output = TransactionOutput(amount, p2sh)
    val tx = BaseTransaction(TransactionConstants.version, Nil, Seq(output), TransactionConstants.lockTime)
    val chan = ChannelAwaitingAnchorTx(tx, lock, Policy.confirmations)
    val inProgress = chan.flatMap(_.clientSign(clientSPK,amount,keys.head))
    val closed = inProgress.flatMap(_.close(serverSPK,keys(1),CurrencyUnits.zero))
    closed.isSuccess must be (true)
    closed.get.serverAmount.get must be (amount)
  }

  it must "be valid for the server to receive all of the money when a payment channel closes in two updates" in {
    val clientSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val serverSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val (lock,keys) = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get
    val p2sh = P2SHScriptPubKey(P2WSHWitnessSPKV0(lock))
    val amount = CurrencyUnits.oneBTC * Satoshis(Int64(2))
    val output = TransactionOutput(amount, p2sh)
    val tx = BaseTransaction(TransactionConstants.version, Nil, Seq(output), TransactionConstants.lockTime)
    val chan = ChannelAwaitingAnchorTx(tx, lock, Policy.confirmations)
    val inProgress: Try[ChannelInProgressClientSigned] = chan.flatMap(_.clientSign(clientSPK,CurrencyUnits.oneBTC,keys.head))
    val serverSign: Try[ChannelInProgress] = inProgress.flatMap(_.serverSign(keys(1)))
    val inProgress2: Try[ChannelInProgressClientSigned] = serverSign.flatMap(_.clientSign(CurrencyUnits.oneBTC,keys.head))
    val closed: Try[ChannelClosedWithEscrow] = inProgress2.flatMap(_.close(serverSPK,keys(1),CurrencyUnits.zero))
    closed.get.serverAmount.get must be (amount)
  }


  it must "fail to close the payment channel if the payment channel's fee is larger than the locked amount" in {
    val clientSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val serverSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val (lock,keys) = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get
    val p2sh = P2SHScriptPubKey(P2WSHWitnessSPKV0(lock))
    val amount = CurrencyUnits.oneBTC
    val output = TransactionOutput(amount, p2sh)
    val tx = BaseTransaction(TransactionConstants.version, Nil, Seq(output), TransactionConstants.lockTime)
    val chan = ChannelAwaitingAnchorTx(tx, lock, Policy.confirmations)
    val inProgress = chan.flatMap(_.clientSign(clientSPK,amount,keys.head))
    //Note the fee here, we try and spend too much money
    val closed = inProgress.flatMap(_.close(serverSPK,keys(1), CurrencyUnits.oneBTC + Satoshis.one))
    closed.isFailure must be (true)
  }

  private def validInProgress: (ChannelInProgress, Seq[ECPrivateKey]) = {
    val clientSPK = ScriptGenerators.p2pkhScriptPubKey.sample.get._1
    val (lock,keys) = ScriptGenerators.escrowTimeoutScriptPubKey2Of2.sample.get
    val witSPK = P2WSHWitnessSPKV0(lock)
    val p2sh = P2SHScriptPubKey(witSPK)
    val amount = CurrencyUnits.oneBTC
    val output = TransactionOutput(amount, p2sh)
    val tc = TransactionConstants
    val tx = BaseTransaction(tc.version, Nil, Seq(output), tc.lockTime)
    val chan = ChannelAwaitingAnchorTx(tx, lock, Policy.confirmations)
    val paymentAmount = Policy.minChannelAmount
    val clientSign = chan.flatMap(_.clientSign(clientSPK,paymentAmount,keys.head))
    val i = clientSign.flatMap(_.serverSign(keys(1)))
    (i.get,keys)
  }
}
