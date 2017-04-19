package org.bitcoins.core.gen

import org.bitcoins.core.channels.{AnchorTransaction, PaymentChannelAwaitingAnchorTx, PaymentChannelInProgress}
import org.bitcoins.core.crypto.ECPrivateKey
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{EscrowTimeoutScriptPubKey, ScriptWitness, WitnessScriptPubKeyV0}
import org.bitcoins.core.protocol.transaction.{TransactionConstants, TransactionOutput}
import org.bitcoins.core.util.BitcoinScriptUtil
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

  def freshPaymentChannelInProgress: Gen[(PaymentChannelInProgress, Seq[ECPrivateKey])] = for {
    (awaiting,privKeys) <- paymentChannelAwaitingAnchorTx
    sequence <- NumberGenerator.uInt32s
    hashType <- CryptoGenerators.hashType
    (s1,_) <- ScriptGenerators.scriptPubKey
    (s2,_) <- ScriptGenerators.scriptPubKey
    o1 = TransactionOutput(awaiting.anchorTx.tx.outputs.head.value,s1)
    o2 = TransactionOutput(CurrencyUnits.zero,s2)
    outputs = Seq(o1,o2)
    unsignedScriptWitness = ScriptWitness(Seq(awaiting.lock.asmBytes))
    unsignedWTxSigComponent = WitnessGenerators.createUnsignedWtxSigComponent(awaiting.scriptPubKey,
      awaiting.amount,unsignedScriptWitness,None,outputs)
    signedScriptSig = WitnessGenerators.csvEscrowTimeoutGenHelper(privKeys,awaiting.lock,unsignedWTxSigComponent,hashType)
    //need to remove the OP_0 or OP_1 and replace it with ScriptNumber.zero / Script
    // Number.one since witnesses are *not* run through the interpreter
    s = BitcoinScriptUtil.minimalDummy(BitcoinScriptUtil.minimalIfOp(signedScriptSig.asm))
    signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(s).reverse
    signedScriptWitness = ScriptWitness(awaiting.lock.asm.flatMap(_.bytes) +: (signedScriptSigPushOpsRemoved.map(_.bytes)))
    (_,signedWtxSigComponent) = WitnessGenerators.createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
    inProgress = PaymentChannelInProgress(awaiting.anchorTx,awaiting.lock,signedWtxSigComponent,Nil)
  } yield (inProgress,privKeys)

}

object ChannelGenerators extends ChannelGenerators
