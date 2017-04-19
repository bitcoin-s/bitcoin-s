package org.bitcoins.core.channels

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._

/**
  * Created by tom on 2/9/17.
  */
sealed trait PaymentChannel {
  /** Commitment transaction initializing the payment channel depositing funds into it. */
  def anchorTx: AnchorTransaction

  def outputIndex: Int = {
    val locks = anchorTx.tx.outputs.zipWithIndex.filter {
      case (o, idx) => o.scriptPubKey.isInstanceOf[EscrowTimeoutScriptPubKey]
    }
    require(locks.length == 1, "We can only have one locking output on a anchor tx, got: " + locks)
    locks.head._2
  }

  def lock: EscrowTimeoutScriptPubKey

  def scriptPubKey: WitnessScriptPubKey = lockingOutput.scriptPubKey.asInstanceOf[WitnessScriptPubKey]

  def lockingOutput: TransactionOutput = anchorTx.tx.outputs(outputIndex)

  def amount: CurrencyUnit = lockingOutput.value
}

sealed trait PaymentChannelAwaitingAnchorTx extends PaymentChannel {
  def confirmations: Long
}

sealed trait PaymentChannelInProgress extends PaymentChannel {
  def currentSpendingTx: WitnessTransaction

  def oldSpendingTxs: Seq[WitnessTransaction]

}

sealed trait PaymentChannelClosed extends PaymentChannel {
  def finalTx: WitnessTransaction

  def oldSpendingTxs: Seq[WitnessTransaction]
}

object PaymentChannelAwaitingAnchorTx {
  private case class PaymentChannelAwaitAnchorTxImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, confirmations: Long) extends PaymentChannelAwaitingAnchorTx

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey): PaymentChannelAwaitingAnchorTx = {
    PaymentChannelAwaitingAnchorTx(anchorTx,lock,0)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, confirmations: Long): PaymentChannelAwaitingAnchorTx = {
    val expectedWitScriptPubKey = WitnessScriptPubKeyV0(lock)
    require(anchorTx.tx.outputs.exists(_.scriptPubKey == expectedWitScriptPubKey),
      "One output on the Anchor Transaction has to have a P2WSH(EscrowTimeoutScriptPubKey)")
    PaymentChannelAwaitAnchorTxImpl(anchorTx,lock,confirmations)
  }
}

object PaymentChannelInProgress {
  private case class PaymentChannelInProgressImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                                  currentSpendingTx: WitnessTransaction,
                                                  oldSpendingTxs: Seq[WitnessTransaction]) extends PaymentChannelInProgress

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, currentSpendingTx: WitnessTransaction): PaymentChannelInProgress = {
    PaymentChannelInProgress(anchorTx,lock,currentSpendingTx,Nil)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, currentSpendingTx: WitnessTransaction,
            oldSpendingTxs: Seq[WitnessTransaction]): PaymentChannelInProgress = {
    PaymentChannelInProgressImpl(anchorTx,lock,currentSpendingTx,oldSpendingTxs)
  }
}


sealed trait PaymentChannelTransaction
case class AnchorTransaction(tx : Transaction) extends PaymentChannelTransaction
