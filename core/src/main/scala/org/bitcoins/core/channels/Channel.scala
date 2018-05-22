package org.bitcoins.core.channels

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{ CurrencyUnit, CurrencyUnits }
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.EscrowTimeoutHelper
import org.bitcoins.core.wallet.builder.TxBuilderError

import scala.annotation.tailrec
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

/**
 * Created by tom on 2/9/17.
 */
sealed abstract class Channel {
  val logger = BitcoinSLogger.logger
  /** Commitment transaction initializing the payment channel depositing funds into it. */
  def anchorTx: Transaction

  /** The index of the output that is the [[EscrowTimeoutScriptPubKey]] in the [[anchorTx]] */
  def outputIndex: Int = {
    val expectedLock = witSPK
    val outputOpt = anchorTx.outputs.zipWithIndex.find {
      case (o, _) =>
        o.scriptPubKey == expectedLock
    }
    require(outputOpt.isDefined, "We do not have the correct locking output on our anchor transasction")
    outputOpt.get._2
  }

  def outPoint = TransactionOutPoint(anchorTx.txId, UInt32(outputIndex))

  /** The [[EscrowTimeoutScriptPubKey]] that needs to be satisfied to spend from the [[anchorTx]] */
  def lock: EscrowTimeoutScriptPubKey

  /** [[WitnessScriptPubKeyV0]] used as the redeem script in the [[P2SHScriptPubKey]] */
  def witSPK = P2WSHWitnessSPKV0(lock)

  def scriptPubKey: P2WSHWitnessSPKV0 = lockingOutput.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0]

  /** The output that we are spending from in the [[Channel]] */
  def lockingOutput: TransactionOutput = anchorTx.outputs(outputIndex)

  /** The total value that is locked up in the [[Channel]] */
  def lockedAmount: CurrencyUnit = lockingOutput.value

}

sealed trait ChannelAwaitingAnchorTx extends Channel {
  /** The number of confirmations on the anchor transaction */
  def confirmations: Long

  /**
   * Creates a [[ChannelInProgress]] from this ChannelAwaitingAnchorTx,
   * starts with [[Policy.minChannelAmount]] being paid to the server
   *
   * HashType is hard coded as SIGHASH_SINGLE|ANYONECANPAY -- this allows the tx to commit to the client's refund
   * output. When the payment is actually redeemed the server will add one output that pays the server
   * and possibly add another input & output that pays a network fee.
   */
  def clientSign(clientChangeSPK: ScriptPubKey, amount: CurrencyUnit,
    privKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[ChannelInProgressClientSigned] = {
    val invariants = Future {
      require(confirmations >= Policy.confirmations, "Need " + Policy.confirmations + " confirmations on the anchor tx before " +
        "we can create a payment channel in progress, got " + confirmations + " confirmations")
      require(amount >= Policy.minChannelAmount, "First payment channel payment amount must be: " + Policy.minChannelAmount + " got: " + amount)
    }

    val o1 = TransactionOutput(lockedAmount - amount, clientChangeSPK)
    val outputs = Seq(o1)
    val partiallySigned = invariants.flatMap { _ =>
      EscrowTimeoutHelper.clientSign(outPoint, anchorTx, outputs, privKey,
        lock, HashType.sigHashSingleAnyoneCanPay)
    }
    val inProgress = partiallySigned.map(tx => ChannelInProgressClientSigned(anchorTx, lock, clientChangeSPK, tx, Nil))
    inProgress
  }

  /**
   * Useful for the server to create a [[org.bitcoins.core.channels.ChannelInProgressClientSigned]]
   * after we receive a partially signed transaction from the client.
   * If None is returned, that means we did not find an output that has the clientSPK
   */
  def createClientSigned(partiallySigned: WitnessTransaction, clientChangeSPK: ScriptPubKey): Option[ChannelInProgressClientSigned] = {
    val inputOpt = partiallySigned.inputs.zipWithIndex.find(_._1.previousOutput.txId == anchorTx.txId)
    val inputIndex = inputOpt.map(i => UInt32(i._2))
    val txSigComponent = inputIndex.map { i =>
      WitnessTxSigComponent(partiallySigned, i, lockingOutput, Policy.standardScriptVerifyFlags)
    }
    txSigComponent.map(t => ChannelInProgressClientSigned(anchorTx, lock, clientChangeSPK, t, Nil))
  }

  /**
   * Attempts to close the [[Channel]] because the [[org.bitcoins.core.protocol.script.EscrowTimeoutScriptPubKey]]
   * has timed out.
   * Note that this does not require any confirmations on the anchor tx,
   * this is because the Client is essentially refunding himself the money
   */
  def closeWithTimeout(clientChangeSPK: ScriptPubKey, clientKey: ECPrivateKey,
    fee: CurrencyUnit)(implicit ec: ExecutionContext): Future[ChannelClosedWithTimeout] = {
    ChannelClosedWithTimeout.closeWithTimeout(this, clientChangeSPK, clientKey, fee)
  }

}

/** This trait represents shared information between [[ChannelInProgress]] and [[ChannelInProgressClientSigned]] */
sealed trait BaseInProgress { this: Channel =>

  /** The most recent [[TxSigComponent]] in the payment channel */
  def current: WitnessTxSigComponent

  /**
   * The previous states of the payment channel.
   * The first item in the Seq is the most recent [[TxSigComponent]] in the [[Channel]]
   */
  def old: Seq[WitnessTxSigComponent]

  /** The [[ScriptPubKey]] that pays the client it's change */
  def clientChangeSPK: ScriptPubKey

  /** The output index that pays change to the client on the spending transaction */
  def clientOutputIndex: Option[Int] = {
    val outputOpt = current.transaction.outputs.zipWithIndex.find {
      case (o, _) => o.scriptPubKey == clientChangeSPK
    }
    outputOpt.map(_._2)
  }

  /** The output that pays change back to the client */
  def clientOutput: Option[TransactionOutput] = clientOutputIndex.map(idx => current.transaction.outputs(idx))

  /** The amount that will be refunded to the client */
  def clientAmount: Option[CurrencyUnit] = clientOutput.map(_.value)

  /**
   * The amount the server will be paid, note this amount will not be the exact amount paid
   * to the server because the network's transaction fee is deducted from the server output
   */
  def serverAmount: Option[CurrencyUnit] = clientAmount.map(a => lockedAmount - a)

  /**
   * Attempts to close the [[Channel]] because the [[EscrowTimeoutScriptPubKey]]
   * has timed out
   */
  def closeWithTimeout(clientKey: ECPrivateKey, fee: CurrencyUnit)(implicit ec: ExecutionContext): Future[ChannelClosedWithTimeout] = {
    ChannelClosedWithTimeout.closeWithTimeout(this, clientChangeSPK, clientKey, fee)
  }
}

/** Represents the state of a Channel transferring money from the client to the server */
sealed trait ChannelInProgress extends Channel with BaseInProgress {

  /** Increments a payment to the server in a [[ChannelInProgress]] */
  def clientSign(amount: CurrencyUnit, clientKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[ChannelInProgressClientSigned] = {
    val outputs = current.transaction.outputs
    val inputs = current.transaction.inputs
    val inputIndex = current.inputIndex
    val client = clientOutput
    val newClient: Try[Option[TransactionOutput]] = client match {
      case Some(c) =>
        val newClientAmount = c.value - amount
        if (newClientAmount < CurrencyUnits.zero) Failure(new IllegalArgumentException("Client is spending more money than was locked in the output"))
        else if (newClientAmount <= Policy.dustThreshold) Success(None)
        else Success(Some(TransactionOutput(newClientAmount, c.scriptPubKey)))
      case None =>
        Failure(new IllegalArgumentException("Client has already spent all of it's money to the server"))
    }
    val newOutputs: Try[Seq[TransactionOutput]] = clientOutputIndex match {
      case Some(idx) => newClient.map {
        case Some(c) => outputs.updated(idx, c)
        case None =>
          //remove old client output
          outputs.patch(idx, Nil, 1)
      }
      case None => Success(outputs)
    }

    val result = Future.fromTry(newOutputs).flatMap { os =>
      val check = checkAmounts(os)
      check match {
        case Success(_) => updateChannel(outPoint, anchorTx, os, clientKey)
        case Failure(err) => Future.failed(err)
      }
    }
    result
  }

  /** Check that the amounts for the given outputs are valid */
  private def checkAmounts(outputs: Seq[TransactionOutput]): Try[Unit] = {
    @tailrec
    def loop(remaining: Seq[TransactionOutput]): Try[Unit] = {
      if (remaining.isEmpty) Success(Unit)
      else {
        val output = remaining.head
        if (output.value > lockedAmount) {
          TxBuilderError.MintsMoney
        } else if (output.value < Policy.dustThreshold) {
          TxBuilderError.OutputBelowDustThreshold
        } else loop(remaining.tail)
      }
    }
    loop(outputs)
  }

  /** Updates the payment channel with the given parameters */
  private def updateChannel(outPoint: TransactionOutPoint, creditingTx: Transaction,
    outputs: Seq[TransactionOutput],
    clientKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[ChannelInProgressClientSigned] = {
    val partiallySigned = EscrowTimeoutHelper.clientSign(outPoint, creditingTx, outputs,
      clientKey, lock, HashType.sigHashSingleAnyoneCanPay)
    val inProgress = partiallySigned.map(tx =>
      ChannelInProgressClientSigned(anchorTx, lock, clientChangeSPK, tx, current +: old))
    inProgress
  }

  /**
   * Useful for the server to create a [[org.bitcoins.core.channels.ChannelInProgressClientSigned]]
   * after we receive a partially signed transaction from the client
   */
  def createClientSigned(partiallySigned: WitnessTransaction): ChannelInProgressClientSigned = {
    val txSigComponent = WitnessTxSigComponent(partiallySigned, current.inputIndex, lockingOutput,
      current.flags)
    ChannelInProgressClientSigned(anchorTx, lock, clientChangeSPK, txSigComponent, current +: old)
  }

}

/** A payment channel that has been signed by the client, but not signed by the server yet */
sealed trait ChannelInProgressClientSigned extends Channel with BaseInProgress {
  /** The new payment channel transaction that has the clients digital signature but does not have the servers digital signature yet */
  private def partiallySignedTx: WitnessTransaction = current.transaction

  /** Signs the payment channel transaction with the server's [[ECPrivateKey]] */
  def serverSign(serverKey: ECPrivateKey)(implicit ec: ExecutionContext): Future[ChannelInProgress] = {
    val unsignedTxSigComponent = WitnessTxSigComponentRaw(
      partiallySignedTx, current.inputIndex,
      lockingOutput, Policy.standardScriptVerifyFlags)

    val signedTxSigComponent: Future[WitnessTxSigComponentRaw] = EscrowTimeoutHelper.serverSign(
      serverKey,
      unsignedTxSigComponent, HashType.sigHashAllAnyoneCanPay)

    signedTxSigComponent.map { s =>
      ChannelInProgress(anchorTx, lock, clientChangeSPK, s, old)
    }
  }

  /** Closes this payment channel, paying the server's amount to the given [[ScriptPubKey]] */
  def close(serverSPK: ScriptPubKey, serverKey: ECPrivateKey, fee: CurrencyUnit)(implicit ec: ExecutionContext): Future[ChannelClosedWithEscrow] = {
    val c = clientOutput
    val clientAmount = c.map(_.value).getOrElse(CurrencyUnits.zero)
    val serverAmount = lockedAmount - clientAmount - fee
    val serverOutput = TransactionOutput(serverAmount, serverSPK)

    //if the client is refunding itself less than the dust threshold we should just remove the output
    val outputs: Seq[TransactionOutput] = if (clientAmount <= Policy.dustThreshold || c.isEmpty) {
      Seq(serverOutput)
    } else {
      Seq(c.get, serverOutput)
    }
    val invariant = checkCloseOutputs(outputs, fee, serverSPK)
    if (invariant.isFailure) {
      Future.failed(invariant.failed.get)
    } else {
      val oldTx = partiallySignedTx
      val updatedTx = WitnessTransaction(oldTx.version, oldTx.inputs, outputs, oldTx.lockTime, oldTx.witness)
      val txSigComponent = WitnessTxSigComponent(updatedTx, current.inputIndex,
        lockingOutput, current.flags)
      val updatedInProgressClientSigned = ChannelInProgressClientSigned(anchorTx, lock, clientChangeSPK, txSigComponent, old)
      val serverSigned = updatedInProgressClientSigned.serverSign(serverKey)
      serverSigned.map(s => ChannelClosedWithEscrow(s, serverSPK))
    }
  }

  /** Sanity checks for the amounts when closing a payment channel */
  private def checkCloseOutputs(outputs: Seq[TransactionOutput], fee: CurrencyUnit,
    serverSPK: ScriptPubKey): Try[Unit] = {
    logger.debug("Outputs for checking during closing of channel: " + outputs)

    val serverOutput = outputs.find(_.scriptPubKey == serverSPK).get
    val clientOutput = outputs.find(_.scriptPubKey == clientChangeSPK)
    val currencyUnits = outputs.map(_.value)
    val fullOutputValue = currencyUnits.fold(CurrencyUnits.zero)(_ + _)
    val fullAmount = fullOutputValue + fee
    if ((lockedAmount - fullOutputValue) != fee) {
      //probably need to come back and check if it is a high or a low fee
      TxBuilderError.HighFee
    } else if (fee >= Policy.maxFee) {
      TxBuilderError.HighFee
    } else if (serverOutput.value <= (Policy.minChannelAmount - fee)) {
      TxBuilderError.OutputBelowDustThreshold
    } else if (clientOutput.isDefined && clientOutput.get.value < Policy.dustThreshold) {
      TxBuilderError.OutputBelowDustThreshold
    } else if (fullAmount < (lockedAmount - Policy.dustThreshold)) {
      TxBuilderError.FeeToLarge
    } else if (fullOutputValue <= CurrencyUnits.zero) {
      TxBuilderError.UnknownError
    } else Success(Unit)
  }
}

sealed trait ChannelClosed extends Channel with BaseInProgress

sealed trait ChannelClosedWithTimeout extends ChannelClosed

sealed trait ChannelClosedWithEscrow extends ChannelClosed {
  /** The [[ScriptPubKey]] that pays the server */
  def serverSPK: ScriptPubKey

  /** The output that pays the server */
  def serverOutput: TransactionOutput = {
    //Invariant in ChannelClosedImpl states this has to exist
    current.transaction.outputs.find(_.scriptPubKey == serverSPK).get
  }
  /** The amount the server is being paid */
  override def serverAmount: Option[CurrencyUnit] = {
    Some(serverOutput.value)
  }
}

object ChannelAwaitingAnchorTx {
  private case class ChannelAwaitAnchorTxImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
    confirmations: Long) extends ChannelAwaitingAnchorTx {
    private val expectedScriptPubKey = P2WSHWitnessSPKV0(lock)
    require(
      anchorTx.outputs.exists(_.scriptPubKey == expectedScriptPubKey),
      "One output on the Anchor Transaction has to have a P2SH(P2WSH(EscrowTimeoutScriptPubKey))")
    require(lockedAmount >= Policy.minChannelAmount, "We need to lock at least " + Policy.minChannelAmount +
      " in the payment channel, got: " + lockedAmount)
  }

  /**
   * Initializes a payment channel with the given anchor transaction and [[EscrowTimeoutScriptPubKey]]
   * Assumes that the anchor transaction has zero confirmations
   */
  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey): Try[ChannelAwaitingAnchorTx] = {
    ChannelAwaitingAnchorTx(anchorTx, lock, 0)
  }

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, confirmations: Long): Try[ChannelAwaitingAnchorTx] = {
    val expectedScriptPubKey = P2WSHWitnessSPKV0(lock)
    val output = anchorTx.outputs.find(_.scriptPubKey == expectedScriptPubKey)
    if (output.isEmpty) {
      TxBuilderError.WrongWitness
    } else if (output.get.value < Policy.minChannelAmount) {
      TxBuilderError.OutputBelowDustThreshold
    } else Success(ChannelAwaitAnchorTxImpl(anchorTx, lock, confirmations))
  }
}

object ChannelInProgress {
  private case class ChannelInProgressImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
    clientChangeSPK: ScriptPubKey, current: WitnessTxSigComponent,
    old: Seq[WitnessTxSigComponent]) extends ChannelInProgress

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, clientSPK: ScriptPubKey,
    current: WitnessTxSigComponent): ChannelInProgress = {
    ChannelInProgress(anchorTx, lock, clientSPK, current, Nil)
  }

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, clientSPK: ScriptPubKey,
    current: WitnessTxSigComponent, old: Seq[WitnessTxSigComponent]): ChannelInProgress = {
    ChannelInProgressImpl(anchorTx, lock, clientSPK, current, old)
  }
}

object ChannelInProgressClientSigned {
  private case class ChannelInProgressClientSignedImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
    clientChangeSPK: ScriptPubKey, current: WitnessTxSigComponent,
    old: Seq[WitnessTxSigComponent]) extends ChannelInProgressClientSigned

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, clientSPK: ScriptPubKey,
    current: WitnessTxSigComponent): ChannelInProgressClientSigned = {
    ChannelInProgressClientSigned(anchorTx, lock, clientSPK, current, Nil)
  }

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, clientChangeSPK: ScriptPubKey,
    current: WitnessTxSigComponent, old: Seq[WitnessTxSigComponent]): ChannelInProgressClientSigned = {
    ChannelInProgressClientSignedImpl(anchorTx, lock, clientChangeSPK, current, old)
  }

}

object ChannelClosedWithEscrow {
  private case class ChannelClosedWithEscrowImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
    current: WitnessTxSigComponent, old: Seq[WitnessTxSigComponent],
    clientChangeSPK: ScriptPubKey, serverSPK: ScriptPubKey) extends ChannelClosedWithEscrow {
    require(current.transaction.outputs.exists(_.scriptPubKey == serverSPK), "The final transaction must have a SPK that pays the server")
  }

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey, current: WitnessTxSigComponent,
    old: Seq[WitnessTxSigComponent], clientSPK: ScriptPubKey, serverSPK: ScriptPubKey): ChannelClosedWithEscrow = {
    ChannelClosedWithEscrowImpl(anchorTx, lock, current, old, clientSPK, serverSPK)
  }

  def apply(i: ChannelInProgress, serverSPK: ScriptPubKey): ChannelClosedWithEscrow = {
    ChannelClosedWithEscrowImpl(i.anchorTx, i.lock, i.current, i.old, i.clientChangeSPK, serverSPK)
  }
}

object ChannelClosedWithTimeout {
  private case class ChannelClosedWithTimeoutImpl(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
    current: WitnessTxSigComponent, old: Seq[WitnessTxSigComponent],
    clientChangeSPK: ScriptPubKey) extends ChannelClosedWithTimeout {
    require(
      current.transaction.outputs.exists(_.scriptPubKey == clientChangeSPK),
      "Client SPK was not defined on a output. This is SPK that is suppose to refund the client it's money")
  }

  def apply(anchorTx: Transaction, lock: EscrowTimeoutScriptPubKey,
    current: WitnessTxSigComponent, old: Seq[WitnessTxSigComponent],
    clientSPK: ScriptPubKey): ChannelClosedWithTimeout = {
    ChannelClosedWithTimeoutImpl(anchorTx, lock, current, old, clientSPK)
  }

  /**
   * Attempts to close the [[Channel]] because the [[org.bitcoins.core.protocol.script.EscrowTimeoutScriptPubKey]]
   * has timed out.
   * Note that this does not require any confirmations on the anchor tx,
   * this is because the Client is essentially refunding himself the money
   */
  def closeWithTimeout(chan: Channel, clientChangeSPK: ScriptPubKey,
    clientKey: ECPrivateKey, fee: CurrencyUnit)(implicit ec: ExecutionContext): Future[ChannelClosedWithTimeout] = {
    val timeout = chan.lock.timeout
    val scriptNum = timeout.locktime
    val sequence = UInt32(scriptNum.toLong)
    val outputs = Seq(TransactionOutput(chan.lockedAmount - fee, clientChangeSPK))
    val outPoint = TransactionOutPoint(chan.anchorTx.txId, UInt32(chan.outputIndex))
    val input = TransactionInput(outPoint, EmptyScriptSignature, sequence)
    val inputs = Seq(input)
    val inputIndex = UInt32(inputs.indexOf(input))
    val signed: Future[WitnessTxSigComponent] = EscrowTimeoutHelper.closeWithTimeout(inputs, outputs, inputIndex, clientKey,
      chan.lock, chan.lockingOutput, HashType.sigHashSingleAnyoneCanPay)
    signed.map(t => ChannelClosedWithTimeout(chan.anchorTx, chan.lock, t, Nil, clientChangeSPK))
  }
}