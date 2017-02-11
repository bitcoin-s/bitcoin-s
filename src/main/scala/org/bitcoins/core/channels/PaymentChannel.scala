package org.bitcoins.core.channels

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{MultiSignatureScriptPubKey, ScriptPubKey, P2SHScriptPubKey}
import org.bitcoins.core.protocol.transaction._

/**
  * Created by tom on 2/9/17.
  */
sealed trait PaymentChannel {
  /** Commitment transaction initializing the payment channel depositing funds into it. */
  def anchorTx : DepositTransaction

  /** The current transaction representing the most recent activity within the payment channel. */
  def currentSpendingTx : CurrentTransaction

  /** The "backup" transaction that will return funds to the creator of the channel (A) if the counterparty (B)
    *  fails to cooperate. */
  def refundTx : RefundTransaction

}

object PaymentChannel {
  private case class PaymentChannelImpl(anchorTx : DepositTransaction,
                                        currentSpendingTx : CurrentTransaction,
                                        refundTx : RefundTransaction,
                                        currentState : PaymentChannelState) extends PaymentChannel

  def apply(anchorTx : DepositTransaction,
            currentSpendingTx : CurrentTransaction,
            refundTx : RefundTransaction) = PaymentChannelImpl(anchorTx, currentSpendingTx, refundTx, currentState = ???)

  def updateSpendingTx(channel: PaymentChannel,
                       currentSpendingTx : CurrentTransaction) = PaymentChannelImpl(channel.anchorTx, currentSpendingTx, channel.refundTx, currentState = ???)

  def apply(channel : PaymentChannel,
            currentSpendingTx : CurrentTransaction) = updateSpendingTx(channel, currentSpendingTx)

  def createUnsignedAnchorTx(payorPubKey : ECPublicKey, payeePubKey : ECPublicKey, amount : CurrencyUnit) : PaymentChannelTransaction = {
    val p2shScriptPubKey : P2SHScriptPubKey = {
      val multiSigPubKey = MultiSignatureScriptPubKey(2, Seq(payorPubKey, payeePubKey))
      P2SHScriptPubKey(multiSigPubKey)
    }
    val unsignedOutput = TransactionOutput(amount, p2shScriptPubKey)
    val tx = Transaction(TransactionConstants.version, Seq(EmptyTransactionInput), Seq(unsignedOutput), TransactionConstants.lockTime)
    DepositTransaction(tx)
  }


}

sealed trait PaymentChannelTransaction
case class RefundTransaction(tx : Transaction) extends PaymentChannelTransaction
case class DepositTransaction(tx : Transaction) extends PaymentChannelTransaction

//Maybe extend this to RefundTransaction as it is just an updated version of the refundTransaction.
//We still need to keep the original RefundTransaction however, so definitely separate/not the same thing.
case class CurrentTransaction(tx : Transaction) extends PaymentChannelTransaction

sealed trait PaymentChannelState

case object Initiate extends PaymentChannelState
case object Ready extends PaymentChannelState
case object Closed extends PaymentChannelState
case object AwaitingDeposit extends PaymentChannelState
case object ExecuteRefund extends PaymentChannelState
