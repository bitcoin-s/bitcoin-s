package org.bitcoins.core.channels

import org.bitcoins.core.channels.PaymentChannelInProgress.PaymentChannelInProgressImpl
import org.bitcoins.core.crypto.{ECDigitalSignature, ECPrivateKey, ECPublicKey, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.gen.WitnessGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}
import org.bitcoins.core.wallet.EscrowTimeoutHelper
import org.bitcoins.core.wallet.WTxSigComponentHelper

import scala.util.Try

/**
  * Created by tom on 2/9/17.
  */
sealed trait PaymentChannel extends BitcoinSLogger {
  /** Commitment transaction initializing the payment channel depositing funds into it. */
  def anchorTx: AnchorTransaction

  def outputIndex: Int = {
    val locks = anchorTx.tx.outputs.zipWithIndex.filter {
      case (o, idx) => o.scriptPubKey.isInstanceOf[WitnessScriptPubKey]
    }
    require(locks.length == 1, "We can only have one locking output on a anchor tx, got: " + locks)
    val expectedLock = WitnessScriptPubKeyV0(lock)
    val actualLock = locks.head._1.scriptPubKey
    require(actualLock == expectedLock, "Incorrect witness scriptPubkey for lock, got: " + actualLock + " expected: " + expectedLock)
    locks.head._2
  }

  def lock: EscrowTimeoutScriptPubKey

  def scriptPubKey: WitnessScriptPubKey = lockingOutput.scriptPubKey.asInstanceOf[WitnessScriptPubKey]

  def lockingOutput: TransactionOutput = anchorTx.tx.outputs(outputIndex)

  def lockedAmount: CurrencyUnit = lockingOutput.value

}

sealed trait PaymentChannelAwaitingAnchorTx extends PaymentChannel {
  def confirmations: Long

  /** Creates a [[PaymentChannelInProgress]] from this PaymentChannelAwaitingAnchorTx, starts with 0 satoshis being paid to the server */
  def clientSign(clientScriptPubKey: ScriptPubKey, serverScriptPubKey: ScriptPubKey, amount: CurrencyUnit,
                       privKey: ECPrivateKey, hashType: HashType): Try[PaymentChannelInProgressClientSigned] = Try {
    require(confirmations >= Policy.confirmations, "Need " + Policy.confirmations + "on the anchor tx before we can create a payment channel " +
      "in progress, got " + confirmations + " confirmations")
    val o1 = TransactionOutput(anchorTx.tx.outputs.head.value - amount,clientScriptPubKey)
    val o2 = TransactionOutput(amount,serverScriptPubKey)
    val outputs = Seq(o1,o2)
    val outPoint = TransactionOutPoint(anchorTx.tx.txId, UInt32(outputIndex))
    val inputs = Seq(TransactionInput(outPoint,EmptyScriptSignature,TransactionConstants.sequence))
    val partiallySignedWTxSigComponent = EscrowTimeoutHelper.clientSign(inputs,outputs,UInt32.zero,privKey,
      lock,scriptPubKey, lockedAmount, hashType)
    val inProgress = PaymentChannelInProgressClientSigned(anchorTx,lock,partiallySignedWTxSigComponent,Nil,serverScriptPubKey)
    inProgress
  }
}

sealed trait PaymentChannelInProgress extends PaymentChannel {
  def current: WitnessTxSigComponent

  def old: Seq[WitnessTxSigComponent]

  /** This is the [[ScriptPubKey]] the payment for services from the server will be sent */
  def serverScriptPubKey: ScriptPubKey

  /** Closes this payment channel */
  def close: PaymentChannelClosed = PaymentChannelClosed(this)

  /** Increments a payment to the server in a [[PaymentChannelInProgress]] */
  def clientSign(amount: CurrencyUnit, privKey: ECPrivateKey, hashType: HashType): Try[PaymentChannelInProgressClientSigned] = {
    val outputs = current.transaction.outputs
    val inputs = current.transaction.inputs
    val inputIndex = current.inputIndex
    val (client,server) = (outputs.head,outputs(1))
    val newClient = TransactionOutput(client, client.value - amount)
    val newServer = TransactionOutput(server, server.value + amount)
    val newOutputs = Seq(newClient,newServer)
    checkAmounts(newClient,newServer).map(_ => updateChannel(inputs,newOutputs,privKey,inputIndex,hashType))
  }

  private def checkAmounts(clientOutput: TransactionOutput, serverOutput: TransactionOutput): Try[Unit] = Try {
    require(clientOutput.value >= CurrencyUnits.zero, "Client doesn't have enough money to pay server")
    require((clientOutput.value + serverOutput.value) <= lockedAmount, "Client and server payment amounts are greater than the locked amount in the payment channel")
  }

  /** Updates the payment channel with the given parameters */
  private def updateChannel(inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput], privKey: ECPrivateKey,
                            inputIndex: UInt32, hashType: HashType): PaymentChannelInProgressClientSigned = {
    val partiallySignedWTxSigComponent = EscrowTimeoutHelper.clientSign(inputs,outputs,inputIndex,privKey,lock,
      scriptPubKey, lockedAmount, hashType)
    val inProgress = PaymentChannelInProgressClientSigned(anchorTx,lock,partiallySignedWTxSigComponent, current +: old,
      serverScriptPubKey)
    inProgress
  }

  /** Useful for the server to create a [[org.bitcoins.core.channels.PaymentChannelInProgressClientSigned]]
    * after we receive a partially signed transaction from the client
    */
  def createClientSigned(partiallySigned: WitnessTransaction): PaymentChannelInProgressClientSigned = {
    val wtxSigComponent = WitnessTxSigComponent(partiallySigned,current.inputIndex, scriptPubKey,
      current.flags,current.amount)
    PaymentChannelInProgressClientSigned(anchorTx,lock,wtxSigComponent, current +: old,serverScriptPubKey)
  }
}

/** A payment channel that has been signed by the client, but not signed by the server yet */
sealed trait PaymentChannelInProgressClientSigned extends PaymentChannelInProgress {
  /** The new payment channel transaction that has the clients digital signature but does not have the servers digital signature yet */
  def partiallySignedTx: WitnessTransaction = current.transaction

  def serverSign(privKey: ECPrivateKey): Try[PaymentChannelInProgress] = {
    val input : Try[(TransactionInput, Int)] = Try {
      partiallySignedTx.inputs.zipWithIndex.find { case (i, index) =>
        i.previousOutput.txId == anchorTx.tx.txId
      }.get
    }

    val output: Try[(TransactionOutput,Int, EscrowTimeoutScriptPubKey)] = input.map { case (input,index) =>
      val output = anchorTx.tx.outputs(input.previousOutput.vout.toInt)
      val asmBytes = partiallySignedTx.witness.witnesses(index).stack.head
      val cmpct = CompactSizeUInt.calculateCompactSizeUInt(asmBytes)
      val lock = EscrowTimeoutScriptPubKey(cmpct.bytes ++ asmBytes)
      (output,input.previousOutput.vout.toInt, lock)
    }
    val partialWitness: Try[ScriptWitness] = input.map { case (_,index) =>
      partiallySignedTx.witness.witnesses(index)
    }
    val unsignedWtxSigComponent: Try[(WitnessTxSigComponent, EscrowTimeoutScriptPubKey)] = output.flatMap { case (o,outputIndex,lock) =>
      input.map { case (_,inputIndex) =>
        (WitnessTxSigComponent(partiallySignedTx,UInt32(inputIndex),
          anchorTx.tx.outputs(outputIndex).scriptPubKey.asInstanceOf[WitnessScriptPubKey],
          Policy.standardScriptVerifyFlags,o.value),lock)
      }
    }

    val sig: Try[ECDigitalSignature] = unsignedWtxSigComponent.map { case (w,lock) =>
      WitnessGenerators.csvEscrowTimeoutGenSignature(privKey, lock, w, HashType.sigHashAll)
    }

    val fullySignedWitness: Try[ScriptWitness] = partialWitness.flatMap { pw =>
      sig.map { s =>
        val redeem = pw.stack.head
        //.dropRight is to remove the OP_0 null dummy
        //.tail.tail is to remove the OP_TRUE for OP_IF and the redeemScript
        val oldSigs: Seq[ECDigitalSignature] = pw.stack.tail.tail.map(ECDigitalSignature(_)).dropRight(1)
        val sigs: Seq[ECDigitalSignature] = oldSigs ++ Seq(s)
        val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
        val escrow = EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
        val minimal = BitcoinScriptUtil.minimalDummy(BitcoinScriptUtil.minimalIfOp(escrow.asm))
        val signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(minimal).reverse
        val signedScriptWitness = ScriptWitness(redeem +: (signedScriptSigPushOpsRemoved.map(_.bytes)))
        signedScriptWitness
      }
    }

    val signedWtx: Try[WitnessTxSigComponent] = fullySignedWitness.flatMap { signedWit =>
      unsignedWtxSigComponent.map { case (u,_) =>
        WTxSigComponentHelper.createSignedWTxComponent(signedWit,u)._2
      }
    }
    signedWtx.map { wtx =>
      PaymentChannelInProgress(anchorTx,lock,wtx, old,serverScriptPubKey)
    }
  }
}

sealed trait PaymentChannelClosed extends PaymentChannel {
  def finalTx: WitnessTxSigComponent

  def old: Seq[WitnessTxSigComponent]
}

object PaymentChannelAwaitingAnchorTx {
  private case class PaymentChannelAwaitAnchorTxImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                                     confirmations: Long) extends PaymentChannelAwaitingAnchorTx {
    private val expectedWitScriptPubKey = WitnessScriptPubKeyV0(lock)
    require(anchorTx.tx.outputs.exists(_.scriptPubKey == expectedWitScriptPubKey),
      "One output on the Anchor Transaction has to have a P2WSH(EscrowTimeoutScriptPubKey)")
    require(lockedAmount >= Policy.minPaymentChannelAmount, "We need to lock at least " + Policy.minPaymentChannelAmount +
      " in the payment channel, got: " + lockedAmount)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey): Try[PaymentChannelAwaitingAnchorTx] = {
    PaymentChannelAwaitingAnchorTx(anchorTx,lock,0)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, confirmations: Long): Try[PaymentChannelAwaitingAnchorTx] = {
    Try(PaymentChannelAwaitAnchorTxImpl(anchorTx,lock,confirmations))
  }
}

object PaymentChannelInProgress {
  private case class PaymentChannelInProgressImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                                  current: WitnessTxSigComponent, old: Seq[WitnessTxSigComponent],
                                                  serverScriptPubKey: ScriptPubKey) extends PaymentChannelInProgress {
    require(current.transaction.outputs.exists(_.scriptPubKey == serverScriptPubKey),
      "One of the scriptPubKeys in the current transaction must pay the server")
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, current: WitnessTxSigComponent,
            serverScriptPubKey: ScriptPubKey): PaymentChannelInProgress = {
    PaymentChannelInProgress(anchorTx,lock,current,Nil,serverScriptPubKey)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, current: WitnessTxSigComponent,
            old: Seq[WitnessTxSigComponent], serverScriptPubKey: ScriptPubKey): PaymentChannelInProgress = {
    PaymentChannelInProgressImpl(anchorTx,lock,current,old,serverScriptPubKey)
  }
}

object PaymentChannelInProgressClientSigned {
  private case class PaymentChannelInProgressClientSignedImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                                  current: WitnessTxSigComponent, old: Seq[WitnessTxSigComponent],
                                                  serverScriptPubKey: ScriptPubKey) extends PaymentChannelInProgressClientSigned {
    require(current.transaction.outputs.exists(_.scriptPubKey == serverScriptPubKey),
      "One of the scriptPubKeys in the current transaction must pay the server")
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, current: WitnessTxSigComponent,
            serverScriptPubKey: ScriptPubKey): PaymentChannelInProgressClientSigned = {
    PaymentChannelInProgressClientSigned(anchorTx,lock,current,Nil,serverScriptPubKey)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, current: WitnessTxSigComponent,
            old: Seq[WitnessTxSigComponent], serverScriptPubKey: ScriptPubKey): PaymentChannelInProgressClientSigned = {
    PaymentChannelInProgressClientSignedImpl(anchorTx,lock,current,old,serverScriptPubKey)
  }

}

object PaymentChannelClosed {
  private case class PaymentChannelClosedImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                              finalTx: WitnessTxSigComponent, old: Seq[WitnessTxSigComponent], serverScriptPubKey: ScriptPubKey) extends PaymentChannelClosed

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, finalTx: WitnessTxSigComponent,
            old: Seq[WitnessTxSigComponent], serverScriptPubKey: ScriptPubKey): PaymentChannelClosed = {
    PaymentChannelClosedImpl(anchorTx,lock,finalTx,old,serverScriptPubKey)
  }

  def apply(i: PaymentChannelInProgress): PaymentChannelClosed = {
    PaymentChannelClosed(i.anchorTx,i.lock,i.current,i.old,i.serverScriptPubKey)
  }
}


sealed trait PaymentChannelTransaction
case class AnchorTransaction(tx : Transaction) extends PaymentChannelTransaction
