package org.bitcoins.core.channels

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.EscrowTimeoutHelper

import scala.util.Try

/**
  * Created by tom on 2/9/17.
  */
sealed trait PaymentChannel extends BitcoinSLogger {
  /** Commitment transaction initializing the payment channel depositing funds into it. */
  def anchorTx: AnchorTransaction

  /** The index of the output that is the [[EscrowTimeoutScriptPubKey]] in the [[anchorTx]] */
  def outputIndex: Int = {
    val locks = anchorTx.tx.outputs.zipWithIndex.filter {
      case (o, idx) => o.scriptPubKey.isInstanceOf[P2SHScriptPubKey]
    }
    require(locks.length == 1, "We can only have one locking output on a anchor tx, got: " + locks)
    val expectedLock = P2SHScriptPubKey(lock)
    val actualLock = locks.head._1.scriptPubKey
    require(actualLock == expectedLock, "Incorrect witness scriptPubkey for lock, got: " + actualLock + " expected: " + expectedLock)
    locks.head._2
  }
  /** The [[EscrowTimeoutScriptPubKey]] that needs to be satisfied to spend from the [[anchorTx]] */
  def lock: EscrowTimeoutScriptPubKey

  def scriptPubKey: P2SHScriptPubKey = lockingOutput.scriptPubKey.asInstanceOf[P2SHScriptPubKey]

  /** The output that we are spending from in the [[PaymentChannel]] */
  def lockingOutput: TransactionOutput = anchorTx.tx.outputs(outputIndex)

  /** The total value that is locked up in the [[PaymentChannel]] */
  def lockedAmount: CurrencyUnit = lockingOutput.value

}

sealed trait PaymentChannelAwaitingAnchorTx extends PaymentChannel {
  /** The number of confirmations on the anchor transaction */
  def confirmations: Long

  /** Creates a [[PaymentChannelInProgress]] from this PaymentChannelAwaitingAnchorTx,
    * starts with 0 satoshis being paid to the server
    *
    * HashType is hard coded as SIGHASH_SINGLE|ANYONECANPAY -- this allows the tx to commit to the client's refund
    * output. When the payment is actually redeemed the server will add one output that pays the server
    * and possibly add another input & output that pays a network fee.
    * */
  def clientSign(clientScriptPubKey: ScriptPubKey, amount: CurrencyUnit,
                       privKey: ECPrivateKey): Try[PaymentChannelInProgressClientSigned] = Try {
    require(confirmations >= Policy.confirmations, "Need " + Policy.confirmations + " confirmations on the anchor tx before " +
      "we can create a payment channel in progress, got " + confirmations + " confirmations")
    require(amount >= Policy.minPaymentChannelAmount, "First payment channel payment amount must be: " + Policy.minPaymentChannelAmount + " got: " + amount)
    val o1 = TransactionOutput(lockedAmount - amount, clientScriptPubKey)
    val outputs = Seq(o1)
    val outPoint = TransactionOutPoint(anchorTx.tx.txId, UInt32(outputIndex))
    val i1 = TransactionInput(outPoint,EmptyScriptSignature,TransactionConstants.sequence)
    val inputs = Seq(i1)
    val inputIndex = UInt32.zero
    val partiallySignedWTxSigComponent = EscrowTimeoutHelper.clientSign(inputs,outputs,inputIndex,privKey,
      lock,scriptPubKey, HashType.sigHashSingleAnyoneCanPay)
    val inProgress = PaymentChannelInProgressClientSigned(anchorTx,lock,partiallySignedWTxSigComponent,Nil)
    inProgress
  }

  /** Useful for the server to create a [[org.bitcoins.core.channels.PaymentChannelInProgressClientSigned]]
    * after we receive a partially signed transaction from the client
    */
  def createClientSigned(partiallySigned: Transaction, inputIndex: UInt32,
                         flags: Seq[ScriptFlag]): PaymentChannelInProgressClientSigned = {
    val txSigComponent = TxSigComponent(partiallySigned,inputIndex, scriptPubKey,flags)
    PaymentChannelInProgressClientSigned(anchorTx,lock,txSigComponent,Nil)
  }

}
/** Represents the state of a PaymentChannel transferring money from the client to the server */
sealed trait PaymentChannelInProgress extends PaymentChannel {
  /** The most recent [[TxSigComponent]] in the payment channel */
  def current: BaseTxSigComponent

  /** The previous states of the payment channel.
    * The first item in the Seq is the most recent [[TxSigComponent]] in the [[PaymentChannel]]
    */
  def old: Seq[BaseTxSigComponent]

  /** The output index that pays change to the client on the spending transaction */
  private def clientOutputIndex: Int = 0

  /** The output that pays change back to the client */
  def clientOutput: TransactionOutput = current.transaction.outputs(clientOutputIndex)

  /** Increments a payment to the server in a [[PaymentChannelInProgress]] */
  def clientSign(amount: CurrencyUnit, privKey: ECPrivateKey): Try[PaymentChannelInProgressClientSigned] = {
    val outputs = current.transaction.outputs
    val inputs = current.transaction.inputs
    val inputIndex = current.inputIndex
    val client = clientOutput
    val newClient = TransactionOutput(client, client.value - amount)
    val newOutputs = outputs.updated(clientOutputIndex,newClient)
    checkAmounts(newClient).map(_ => updateChannel(inputs,newOutputs,privKey,inputIndex))
  }

  /** Check that the amounts on the outputs are valid */
  private def checkAmounts(output: TransactionOutput): Try[Unit] = Try {
    require(output.value >= CurrencyUnits.zero, "Client doesn't have enough money to pay server")
    require(output.value <= lockedAmount, "Client output cannot have more money than the total locked amount")
  }

  /** Updates the payment channel with the given parameters */
  private def updateChannel(inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput], privKey: ECPrivateKey,
                            inputIndex: UInt32): PaymentChannelInProgressClientSigned = {
    val partiallySignedWTxSigComponent = EscrowTimeoutHelper.clientSign(inputs,outputs,inputIndex,privKey,lock,
      scriptPubKey, HashType.sigHashSingleAnyoneCanPay)
    val inProgress = PaymentChannelInProgressClientSigned(anchorTx,lock,
      partiallySignedWTxSigComponent, current +: old)
    inProgress
  }

  /** Useful for the server to create a [[org.bitcoins.core.channels.PaymentChannelInProgressClientSigned]]
    * after we receive a partially signed transaction from the client
    */
  def createClientSigned(partiallySigned: BaseTransaction): PaymentChannelInProgressClientSigned = {
    val txSigComponent = TxSigComponent(partiallySigned,current.inputIndex, scriptPubKey,
      current.flags)
    PaymentChannelInProgressClientSigned(anchorTx,lock,txSigComponent, current +: old)
  }
}

/** A payment channel that has been signed by the client, but not signed by the server yet */
sealed trait PaymentChannelInProgressClientSigned extends PaymentChannelInProgress {
  /** The new payment channel transaction that has the clients digital signature but does not have the servers digital signature yet */
  def partiallySignedTx: Transaction = current.transaction

  /** Signs the payment channel transaction with the server's [[ECPrivateKey]] */
  def serverSign(privKey: ECPrivateKey): Try[PaymentChannelInProgress] = {
    val unsignedBTxSigComponent: BaseTxSigComponent = TxSigComponent(partiallySignedTx,
      current.inputIndex, lock, Policy.standardScriptVerifyFlags)

    val signedTxSigComponent: Try[BaseTxSigComponent] = EscrowTimeoutHelper.serverSign(privKey, scriptPubKey,
      unsignedBTxSigComponent, HashType.sigHashAll)

    signedTxSigComponent.map { s =>
      PaymentChannelInProgress(anchorTx,lock,s, old)
    }
  }

  /** Closes this payment channel, paying the server's amount to the given [[ScriptPubKey]] */
  def close(serverScriptPubKey: ScriptPubKey, serverPrivKey: ECPrivateKey, fee: CurrencyUnit): Try[PaymentChannelClosed] = {
    val c = clientOutput
    val serverAmount = lockedAmount - c.value - fee

    val invariant = Try(require(serverAmount >= Policy.dustThreshold, "Server amount does not meet dust threshold"))

    val serverOutput = TransactionOutput(serverAmount,serverScriptPubKey)
    val outputs = Seq(c,serverOutput)
    val oldTx = current.transaction
    val updatedTx = Transaction(oldTx.version,oldTx.inputs,outputs,oldTx.lockTime)
    val btxSigComponent = TxSigComponent(updatedTx,current.inputIndex,
      current.scriptPubKey,current.flags)
    val updatedInProgressClientSigned = PaymentChannelInProgressClientSigned(anchorTx,lock,btxSigComponent,old)
    val serverSigned = invariant.flatMap(_ => updatedInProgressClientSigned.serverSign(serverPrivKey))
    serverSigned.map(s => PaymentChannelClosed(s,serverScriptPubKey))
  }
}

sealed trait PaymentChannelClosed extends PaymentChannel {
  /** This is the [[TxSigComponent]] that will be broadcast to the blockchain */
  def finalTx: TxSigComponent

  def old: Seq[BaseTxSigComponent]
}

object PaymentChannelAwaitingAnchorTx {
  private case class PaymentChannelAwaitAnchorTxImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                                     confirmations: Long) extends PaymentChannelAwaitingAnchorTx {
    private val expectedScriptPubKey = P2SHScriptPubKey(lock)
    require(anchorTx.tx.outputs.exists(_.scriptPubKey == expectedScriptPubKey),
      "One output on the Anchor Transaction has to have a P2SH(EscrowTimeoutScriptPubKey)")
    require(lockedAmount >= Policy.minPaymentChannelAmount, "We need to lock at least " + Policy.minPaymentChannelAmount +
      " in the payment channel, got: " + lockedAmount)
  }

  /** Initializes a payment channel with the given anchor transaction and [[EscrowTimeoutScriptPubKey]]
    * Assumes that the anchor transaction has zero confirmations
    */
  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey): Try[PaymentChannelAwaitingAnchorTx] = {
    PaymentChannelAwaitingAnchorTx(anchorTx,lock,0)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, confirmations: Long): Try[PaymentChannelAwaitingAnchorTx] = {
    Try(PaymentChannelAwaitAnchorTxImpl(anchorTx,lock,confirmations))
  }
}

object PaymentChannelInProgress {
  private case class PaymentChannelInProgressImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                                  current: BaseTxSigComponent,
                                                  old: Seq[BaseTxSigComponent]) extends PaymentChannelInProgress


  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
            current: BaseTxSigComponent): PaymentChannelInProgress = {
    PaymentChannelInProgress(anchorTx,lock,current,Nil)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, current: BaseTxSigComponent,
            old: Seq[BaseTxSigComponent]): PaymentChannelInProgress = {
    PaymentChannelInProgressImpl(anchorTx,lock,current,old)
  }
}

object PaymentChannelInProgressClientSigned {
  private case class PaymentChannelInProgressClientSignedImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                                              current: BaseTxSigComponent,
                                                              old: Seq[BaseTxSigComponent]) extends PaymentChannelInProgressClientSigned

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
            current: BaseTxSigComponent): PaymentChannelInProgressClientSigned = {
    PaymentChannelInProgressClientSigned(anchorTx,lock,current,Nil)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, current: BaseTxSigComponent,
            old: Seq[BaseTxSigComponent]): PaymentChannelInProgressClientSigned = {
    PaymentChannelInProgressClientSignedImpl(anchorTx,lock,current,old)
  }

}

object PaymentChannelClosed {
  private case class PaymentChannelClosedImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                              finalTx: BaseTxSigComponent, old: Seq[BaseTxSigComponent], serverScriptPubKey: ScriptPubKey) extends PaymentChannelClosed {
    require(finalTx.transaction.outputs.length > 1, "The final transaction on a PaymentChannelClosed must have two or more outputs, " +
      "one paying the client, and the other paying the server. Got: " + finalTx.transaction.outputs)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, finalTx: BaseTxSigComponent,
            old: Seq[BaseTxSigComponent], serverScriptPubKey: ScriptPubKey): PaymentChannelClosed = {
    PaymentChannelClosedImpl(anchorTx,lock,finalTx,old,serverScriptPubKey)
  }

  def apply(i: PaymentChannelInProgress, serverScriptPubKey: ScriptPubKey): PaymentChannelClosed = {
    PaymentChannelClosed(i.anchorTx,i.lock,i.current,i.old,serverScriptPubKey)
  }
}


sealed trait PaymentChannelTransaction
case class AnchorTransaction(tx : Transaction) extends PaymentChannelTransaction
