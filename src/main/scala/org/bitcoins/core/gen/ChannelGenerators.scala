package org.bitcoins.core.gen

import org.bitcoins.core.channels.{AnchorTransaction, PaymentChannelAwaitingAnchorTx, PaymentChannelInProgress}
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{EscrowTimeoutScriptPubKey, WitnessScriptPubKeyV0}
import org.bitcoins.core.protocol.transaction.{TransactionConstants, TransactionOutput}
import org.scalacheck.Gen

/**
  * Created by chris on 4/18/17.
  */
trait ChannelGenerators {

  def anchorTx: Gen[(AnchorTransaction, EscrowTimeoutScriptPubKey, Seq[ECPrivateKey])] = for {
    (redeemScript,privKeys) <- ScriptGenerators.escrowTimeoutScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    wit = WitnessScriptPubKeyV0(redeemScript)
    (aTx,_) = TransactionGenerators.buildCreditingTransaction(UInt32(2),wit,amount)
  } yield (AnchorTransaction(aTx),redeemScript,privKeys)

  def paymentChannelAwaitingAnchorTx: Gen[(PaymentChannelAwaitingAnchorTx, Seq[ECPrivateKey])] = for {
    (aTx,redeemScript,privKeys) <- anchorTx
    confs <- Gen.posNum[Long]
  } yield (PaymentChannelAwaitingAnchorTx(aTx,redeemScript,confs),privKeys)

/*  def freshPaymentChannelInProgress: Gen[(PaymentChannelInProgress, Seq[ECPrivateKey])] = for {
    (awaiting,privKeys) <- paymentChannelAwaitingAnchorTx
    sequence <- NumberGenerator.uInt32s
    (s1,_) <- ScriptGenerators.scriptPubKey
    (s2,_) <- ScriptGenerators.scriptPubKey
    o1 = TransactionOutput(awaiting.anchorTx.tx.outputs.head.value,s1)
    o2 = TransactionOutput(CurrencyUnits.zero,s2)
    outputs = Seq(o1,o2)
    (scriptSig,_,_) <- ScriptGenerators.signedMultiSigEscrowTimeoutScriptSig(awaiting.lock,privKeys,
      sequence,outputs,awaiting.amount)
    (spendingTx, inputIndex) = TransactionGenerators.buildSpendingWitnessTransaction(UInt32(2),awaiting.anchorTx.tx,scriptSig,
      UInt32(awaiting.outputIndex),TransactionConstants.lockTime,sequence,outputs)
    inProgress = PaymentChannelInProgress(awaiting.anchorTx,spendingTx,Nil)
  } yield (inProgress,privKeys)*/

}

object ChannelGenerators extends ChannelGenerators
