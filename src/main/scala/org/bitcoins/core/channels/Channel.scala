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
sealed trait Channel extends BitcoinSLogger {
  /** Commitment transaction initializing the payment channel depositing funds into it. */
  def anchorTx: Transaction

  /** The index of the output that is the [[EscrowTimeoutScriptPubKey]] in the [[anchorTx]] */
  def outputIndex: Int = {
    val expectedLock = P2SHScriptPubKey(lock)
    val locks = anchorTx.outputs.zipWithIndex.filter {
      case (o, _) =>
        o.scriptPubKey == expectedLock
    }
    require(locks.length == 1, "We can only have one locking output on a anchor tx, got: " + locks)

    val actualLock = locks.head._1.scriptPubKey
    require(actualLock == expectedLock, "Incorrect scriptPubkey for lock, got: " + actualLock + " expected: " + expectedLock)
    locks.head._2
  }
  /** The [[EscrowTimeoutScriptPubKey]] that needs to be satisfied to spend from the [[anchorTx]] */
  def lock: EscrowTimeoutScriptPubKey

  def scriptPubKey: P2SHScriptPubKey = lockingOutput.scriptPubKey.asInstanceOf[P2SHScriptPubKey]

  /** The output that we are spending from in the [[Channel]] */
  def lockingOutput: TransactionOutput = anchorTx.outputs(outputIndex)

  /** The total value that is locked up in the [[Channel]] */
  def lockedAmount: CurrencyUnit = lockingOutput.value

}

sealed trait ChannelAwaitingAnchorTx extends Channel {
  /** The number of confirmations on the anchor transaction */
  def confirmations: Long

  /** Creates a [[ChannelInProgress]] from this ChannelAwaitingAnchorTx,
    * starts with [[Policy.minChannelAmount]] being paid to the server
    *
    * HashType is hard coded as SIGHASH_SINGLE|ANYONECANPAY -- this allows the tx to commit to the client's refund
    * output. When the payment is actually redeemed the server will add one output that pays the server
    * and possibly add another input & output that pays a network fee.
    * */
  def clientSign(clientSPK: ScriptPubKey, amount: CurrencyUnit,
                       privKey: ECPrivateKey): Try[ChannelInProgressClientSigned] = Try {
    require(confirmations >= Policy.confirmations, "Need " + Policy.confirmations + " confirmations on the anchor tx before " +
      "we can create a payment channel in progress, got " + confirmations + " confirmations")
    require(amount >= Policy.minChannelAmount, "First payment channel payment amount must be: " + Policy.minChannelAmount + " got: " + amount)
    val o1 = TransactionOutput(lockedAmount - amount, clientSPK)
    val outputs = Seq(o1)
    val outPoint = TransactionOutPoint(anchorTx.txId, UInt32(outputIndex))
    val i1 = TransactionInput(outPoint,EmptyScriptSignature,TransactionConstants.sequence)
    val inputs = Seq(i1)
    val inputIndex = UInt32.zero
    val partiallySignedWTxSigComponent = EscrowTimeoutHelper.clientSign(inputs,outputs,inputIndex,privKey,
      lock,scriptPubKey, HashType.sigHashSingleAnyoneCanPay)
    val inProgress = ChannelInProgressClientSigned(anchorTx,lock,clientSPK,partiallySignedWTxSigComponent,Nil)
    inProgress
  }

  /** Useful for the server to create a [[org.bitcoins.core.channels.ChannelInProgressClientSigned]]
    * after we receive a partially signed transaction from the client
    */
  def createClientSigned(partiallySigned: Transaction, clientSPK: ScriptPubKey): Option[ChannelInProgressClientSigned] = {
    val inputOpt = partiallySigned.inputs.zipWithIndex.find(_._1.previousOutput.txId == anchorTx.txId)
    val inputIndex = inputOpt.map(i => UInt32(i._2))
    val txSigComponent = inputIndex.map(i => TxSigComponent(partiallySigned, i, scriptPubKey,Policy.standardScriptVerifyFlags))
    txSigComponent.map(t => ChannelInProgressClientSigned(anchorTx,lock,clientSPK,t,Nil))
  }

}
/** Represents the state of a Channel transferring money from the client to the server */
sealed trait ChannelInProgress extends Channel {
  /** The most recent [[TxSigComponent]] in the payment channel */
  def current: BaseTxSigComponent

  /** The previous states of the payment channel.
    * The first item in the Seq is the most recent [[TxSigComponent]] in the [[Channel]]
    */
  def old: Seq[BaseTxSigComponent]

  /** The [[ScriptPubKey]] that pays the client it's refund */
  def clientSPK: ScriptPubKey

  /** The output index that pays change to the client on the spending transaction */
  private def clientOutputIndex: Int = {
    val output = current.transaction.outputs.find(_.scriptPubKey == clientSPK)
    //TODO: Think about this invariant when the client has paid ALL of their money to the server
    require(output.isDefined, "The spending transaction must have an output that pays the client's SPK")
    val idx = current.transaction.outputs.indexOf(output.get)
    require(idx != -1, "Cannot have the index undefined")
    idx
  }

  /** The output that pays change back to the client */
  def clientOutput: TransactionOutput = current.transaction.outputs(clientOutputIndex)

  /** Increments a payment to the server in a [[ChannelInProgress]] */
  def clientSign(amount: CurrencyUnit, clientKey: ECPrivateKey): Try[ChannelInProgressClientSigned] = {
    val outputs = current.transaction.outputs
    val inputs = current.transaction.inputs
    val inputIndex = current.inputIndex
    val client = clientOutput
    val newClient = TransactionOutput(client, client.value - amount)
    val newOutputs = outputs.updated(clientOutputIndex,newClient)
    checkAmounts(newClient).map(_ => updateChannel(inputs,newOutputs,clientKey,inputIndex))
  }

  /** Check that the amounts on the outputs are valid */
  private def checkAmounts(output: TransactionOutput): Try[Unit] = Try {
    require(output.value >= CurrencyUnits.zero, "Client doesn't have enough money to pay server")
    require(output.value <= lockedAmount, "Client output cannot have more money than the total locked amount")
  }

  /** Updates the payment channel with the given parameters */
  private def updateChannel(inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput], clientKey: ECPrivateKey,
                            inputIndex: UInt32): ChannelInProgressClientSigned = {
    val partiallySignedWTxSigComponent = EscrowTimeoutHelper.clientSign(inputs,outputs,inputIndex,clientKey,lock,
      scriptPubKey, HashType.sigHashSingleAnyoneCanPay)
    val inProgress = ChannelInProgressClientSigned(anchorTx,lock,clientSPK,
      partiallySignedWTxSigComponent, current +: old)
    inProgress
  }

  /** Useful for the server to create a [[org.bitcoins.core.channels.ChannelInProgressClientSigned]]
    * after we receive a partially signed transaction from the client
    */
  def createClientSigned(partiallySigned: BaseTransaction): ChannelInProgressClientSigned = {
    val txSigComponent = TxSigComponent(partiallySigned,current.inputIndex, scriptPubKey,
      current.flags)
    ChannelInProgressClientSigned(anchorTx,lock, clientSPK,txSigComponent, current +: old)
  }
}

/** A payment channel that has been signed by the client, but not signed by the server yet */
sealed trait ChannelInProgressClientSigned extends Channel {
  /** The [[org.bitcoins.core.crypto.BaseTxSigComponent]] that was partially signed by the client */
  def partiallySigned: BaseTxSigComponent

  def old: Seq[BaseTxSigComponent]

  /** The [[ScriptPubKey]] that pays the client it's refund */
  def clientSPK: ScriptPubKey

  /** The output index that pays change to the client on the spending transaction */
  private def clientOutputIndex: Int = {
    val output = partiallySignedTx.outputs.find(_.scriptPubKey == clientSPK)
    //TODO: Think about this invariant when the client has paid ALL of their money to the server
    require(output.isDefined, "The spending transaction must have an output that pays the client's SPK")
    val idx = partiallySignedTx.outputs.indexOf(output.get)
    require(idx != -1, "Cannot have the index undefined")
    idx
  }

  /** The output that pays change back to the client */
  def clientOutput: TransactionOutput = partiallySignedTx.outputs(clientOutputIndex)

  /** The new payment channel transaction that has the clients digital signature but does not have the servers digital signature yet */
  private def partiallySignedTx: Transaction = partiallySigned.transaction

  /** Signs the payment channel transaction with the server's [[ECPrivateKey]] */
  def serverSign(serverKey: ECPrivateKey): Try[ChannelInProgress] = {
    val unsignedBTxSigComponent: BaseTxSigComponent = TxSigComponent(partiallySignedTx,
      partiallySigned.inputIndex, lock, Policy.standardScriptVerifyFlags)

    val signedTxSigComponent: Try[BaseTxSigComponent] = EscrowTimeoutHelper.serverSign(serverKey, scriptPubKey,
      unsignedBTxSigComponent, HashType.sigHashAll)

    signedTxSigComponent.map { s =>
      ChannelInProgress(anchorTx,lock, clientSPK, s, old)
    }
  }

  /** Closes this payment channel, paying the server's amount to the given [[ScriptPubKey]] */
  def close(serverSPK: ScriptPubKey, serverKey: ECPrivateKey, fee: CurrencyUnit): Try[ChannelClosed] = {
    val c = clientOutput
    val serverAmount = lockedAmount - c.value - fee
    val serverOutput = TransactionOutput(serverAmount,serverSPK)

    //if the client is refunding itself less than the dust threshold we should just remove the output
    val outputs = if (c.value <= Policy.dustThreshold) {
      Seq(serverOutput)
    } else Seq(c,serverOutput)
    val invariant = checkCloseOutputs(outputs,fee, serverSPK)
    val oldTx = partiallySignedTx
    val updatedTx = Transaction(oldTx.version,oldTx.inputs,outputs,oldTx.lockTime)
    val btxSigComponent = TxSigComponent(updatedTx,partiallySigned.inputIndex,
      partiallySigned.scriptPubKey,partiallySigned.flags)
    val updatedInProgressClientSigned = ChannelInProgressClientSigned(anchorTx,lock,clientSPK,btxSigComponent,old)
    val serverSigned = invariant.flatMap(_ => updatedInProgressClientSigned.serverSign(serverKey))
    serverSigned.map(s => ChannelClosed(s,serverSPK))
  }

  /** Sanity checks for the amounts when closing a payment channel */
  private def checkCloseOutputs(outputs: Seq[TransactionOutput], fee: CurrencyUnit, serverSPK: ScriptPubKey):Try[Unit] = Try {
    val serverOutput = outputs.find(_.scriptPubKey == serverSPK).get
    val clientOutput = outputs.find(_.scriptPubKey == clientSPK)
    require(serverOutput.value >= Policy.minChannelAmount, "Server amount does not meet Policy.minChannelAmount, got: " + serverOutput.value)
    clientOutput.map(o => require(o.value >= Policy.dustThreshold, "Client output amount must be dust threshold, got: " + o.value))
    val fullAmount = clientOutput.map(_.value).getOrElse(CurrencyUnits.zero) + serverOutput.value + fee
    require(fullAmount == lockedAmount, "Losing satoshis some when closing the channel")
  }
}

sealed trait ChannelClosed extends Channel {
  /** This is the [[TxSigComponent]] that will be broadcast to the blockchain */
  def finalTx: TxSigComponent

  def old: Seq[BaseTxSigComponent]

  def serverSPK: ScriptPubKey

  def clientSPK: ScriptPubKey

  def serverOutput: Option[TransactionOutput] = finalTx.transaction.outputs.find(_.scriptPubKey == serverSPK)

  def serverAmount: Option[CurrencyUnit] = serverOutput.map(_.value)

  def clientOutput: Option[TransactionOutput] = finalTx.transaction.outputs.find(_.scriptPubKey == clientSPK)

  def clientValue: Option[CurrencyUnit] = clientOutput.map(_.value)
}

object ChannelAwaitingAnchorTx {
  private case class ChannelAwaitAnchorTxImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
                                                     confirmations: Long) extends ChannelAwaitingAnchorTx {
    private val expectedScriptPubKey = P2SHScriptPubKey(lock)
    require(anchorTx.outputs.exists(_.scriptPubKey == expectedScriptPubKey),
      "One output on the Anchor Transaction has to have a P2SH(EscrowTimeoutScriptPubKey)")
    require(lockedAmount >= Policy.minChannelAmount, "We need to lock at least " + Policy.minChannelAmount +
      " in the payment channel, got: " + lockedAmount)
  }

  /** Initializes a payment channel with the given anchor transaction and [[EscrowTimeoutScriptPubKey]]
    * Assumes that the anchor transaction has zero confirmations
    */
  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey): Try[ChannelAwaitingAnchorTx] = {
    ChannelAwaitingAnchorTx(anchorTx,lock,0)
  }

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, confirmations: Long): Try[ChannelAwaitingAnchorTx] = {
    Try(ChannelAwaitAnchorTxImpl(anchorTx,lock,confirmations))
  }
}

object ChannelInProgress {
  private case class ChannelInProgressImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
                                                  clientSPK: ScriptPubKey, current: BaseTxSigComponent,
                                                  old: Seq[BaseTxSigComponent]) extends ChannelInProgress


  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, clientSPK: ScriptPubKey,
            current: BaseTxSigComponent): ChannelInProgress = {
    ChannelInProgress(anchorTx,lock,clientSPK, current,Nil)
  }

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, clientSPK: ScriptPubKey,
    current: BaseTxSigComponent, old: Seq[BaseTxSigComponent]): ChannelInProgress = {
    ChannelInProgressImpl(anchorTx,lock,clientSPK,current,old)
  }
}

object ChannelInProgressClientSigned {
  private case class ChannelInProgressClientSignedImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
                                                              clientSPK: ScriptPubKey, partiallySigned: BaseTxSigComponent,
                                                              old: Seq[BaseTxSigComponent]) extends ChannelInProgressClientSigned

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, clientSPK: ScriptPubKey,
            current: BaseTxSigComponent): ChannelInProgressClientSigned = {
    ChannelInProgressClientSigned(anchorTx,lock, clientSPK, current,Nil)
  }

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, clientSPK: ScriptPubKey,
            current: BaseTxSigComponent, old: Seq[BaseTxSigComponent]): ChannelInProgressClientSigned = {
    ChannelInProgressClientSignedImpl(anchorTx,lock, clientSPK, current,old)
  }

}

object ChannelClosed {
  private case class ChannelClosedImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
                                       finalTx: BaseTxSigComponent, old: Seq[BaseTxSigComponent],
                                       clientSPK: ScriptPubKey, serverSPK: ScriptPubKey) extends ChannelClosed

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, finalTx: BaseTxSigComponent,
            old: Seq[BaseTxSigComponent], clientSPK: ScriptPubKey, serverSPK: ScriptPubKey): ChannelClosed = {
    ChannelClosedImpl(anchorTx,lock,finalTx,old,clientSPK, serverSPK)
  }

  def apply(i: ChannelInProgress, serverSPK: ScriptPubKey): ChannelClosed = {
    ChannelClosed(i.anchorTx,i.lock,i.current,i.old,i.clientSPK,serverSPK)
  }
}
