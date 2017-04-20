package org.bitcoins.core.channels

import org.bitcoins.core.crypto.{ECPrivateKey, ECPublicKey, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.gen.WitnessGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinScriptUtil

/**
  * Created by tom on 2/9/17.
  */
sealed trait PaymentChannel {
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
}

sealed trait PaymentChannelInProgress extends PaymentChannel {
  def current: WitnessTxSigComponent

  def old: Seq[WitnessTxSigComponent]

  /** This is the [[ScriptPubKey where]] the payment for services from the server will be sent */
  def serverScriptPubKey: ScriptPubKey


  /** Closes this payment channel */
  def close: PaymentChannelClosed = PaymentChannelClosed(this)

  /** Increments a payment to the server in a [[PaymentChannelInProgress]] */
  def increment(amount: CurrencyUnit, privKeys: Seq[ECPrivateKey], hashType: HashType): PaymentChannelInProgress = {
    val outputs = current.transaction.outputs
    val (client,server) = (outputs.head,outputs(1))
    val newClient = TransactionOutput(client, client.value - amount)
    val newServer = TransactionOutput(server, server.value + amount)
    updateChannel(Seq(newClient,newServer),privKeys,hashType)
  }

  /** Updates the payment channel with the given parameters */
  private def updateChannel(outputs: Seq[TransactionOutput], privKeys: Seq[ECPrivateKey], hashType: HashType): PaymentChannelInProgress = {
    val unsignedScriptWitness = ScriptWitness(Seq(lock.asmBytes))
    val unsignedWTxSigComponent = WitnessGenerators.createUnsignedWtxSigComponent(scriptPubKey,
      lockedAmount,unsignedScriptWitness,None,outputs)
    val signedScriptSig = WitnessGenerators.csvEscrowTimeoutGenHelper(privKeys,lock,unsignedWTxSigComponent,hashType)
    val s = BitcoinScriptUtil.minimalDummy(BitcoinScriptUtil.minimalIfOp(signedScriptSig.asm))
    val signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(s).reverse
    val signedScriptWitness = ScriptWitness(lock.asm.flatMap(_.bytes) +: (signedScriptSigPushOpsRemoved.map(_.bytes)))
    val (_,signedWtxSigComponent) = WitnessGenerators.createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
    val inProgress = PaymentChannelInProgress(anchorTx,lock,signedWtxSigComponent,current +: old,
      serverScriptPubKey)
    inProgress
  }
}

sealed trait PaymentChannelClosed extends PaymentChannel {
  def finalTx: WitnessTxSigComponent

  def old: Seq[WitnessTxSigComponent]
}

object PaymentChannelAwaitingAnchorTx {
  private case class PaymentChannelAwaitAnchorTxImpl(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey,
                                                     confirmations: Long) extends PaymentChannelAwaitingAnchorTx {
    val expectedWitScriptPubKey = WitnessScriptPubKeyV0(lock)
    require(anchorTx.tx.outputs.exists(_.scriptPubKey == expectedWitScriptPubKey),
      "One output on the Anchor Transaction has to have a P2WSH(EscrowTimeoutScriptPubKey)")
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey): PaymentChannelAwaitingAnchorTx = {
    PaymentChannelAwaitingAnchorTx(anchorTx,lock,0)
  }

  def apply(anchorTx: AnchorTransaction, lock: EscrowTimeoutScriptPubKey, confirmations: Long): PaymentChannelAwaitingAnchorTx = {
    PaymentChannelAwaitAnchorTxImpl(anchorTx,lock,confirmations)
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
