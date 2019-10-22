package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{ECPrivateKey, TransactionSignatureCreator, WitnessTxSigComponentRaw}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.testkit.core.gen.{CryptoGenerators, CurrencyUnitGenerator, NumberGenerator, TransactionGenerators}
import org.scalacheck.Gen

sealed abstract class HTLCWitnessGenerators {
  private val tc = TransactionConstants
  type WitComponent = (TransactionWitness, WitnessTxSigComponentRaw,Seq[ECPrivateKey])
  def signedReceivedHTLCTimeoutWitness: Gen[WitComponent] = for {
    (scriptNum,lockTime) <- TransactionGenerators.spendableCLTVValues
    (spk,privKeys) <- HTLCGenerators.receivedHTLC(scriptNum)
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witSPK = P2WSHWitnessSPKV0(spk)
    uScriptWitness = P2WSHWitnessV0(spk)
    output <- TransactionGenerators.output
    (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(spk,amount)
    input = TransactionInput(TransactionOutPoint(creditingTx.txId, outputIndex),EmptyScriptSignature,UInt32.zero)
    uWTx = WitnessTransaction(tc.validLockVersion,Seq(input),Seq(output),
      lockTime,TransactionWitness(Vector(uScriptWitness)))
    u = WitnessTxSigComponentRaw(uWTx,UInt32.zero,output,Policy.standardFlags)
    sig = TransactionSignatureCreator.createSig(u,privKeys(1),hashType)
    ss = ReceivedHTLCScriptSig(sig)
    signedScriptWitness = P2WSHWitnessV0(spk, ss)
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt,signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version,oldTx.inputs,oldTx.outputs,oldTx.lockTime,txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,u.inputIndex,output,u.flags)
  } yield (txWitness,signedWtxSigComponent,privKeys)

  def signedReceivedHTLCRevokedWitness: Gen[WitComponent] = for {
    lockTime <- NumberGenerator.uInt32s
    (spk,privKeys) <- HTLCGenerators.receivedHTLC
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witSPK = P2WSHWitnessSPKV0(spk)
    uScriptWitness = P2WSHWitnessV0(spk)
    output <- TransactionGenerators.output
    (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(spk,amount)
    input = TransactionInput(TransactionOutPoint(creditingTx.txId, outputIndex),EmptyScriptSignature,UInt32.zero)
    uWTx = WitnessTransaction(tc.validLockVersion,Seq(input),Seq(output),
      lockTime,TransactionWitness(Vector(uScriptWitness)))
    u = WitnessTxSigComponentRaw(uWTx,UInt32.zero,output,Policy.standardFlags)
    sig = TransactionSignatureCreator.createSig(u,privKeys(0),hashType)
    ss = ReceivedHTLCScriptSig(sig,privKeys(0).publicKey)
    signedScriptWitness = P2WSHWitnessV0(spk, ss)
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt,signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version,oldTx.inputs,oldTx.outputs,oldTx.lockTime,txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,u.inputIndex,output,u.flags)
  } yield (txWitness,signedWtxSigComponent,privKeys)

  //TODO: Need to add generator for received htlc for when a received htlc is a success

  def signedOfferedHTLCRevocationWitness: Gen[WitComponent] = for {
    lockTime <- NumberGenerator.uInt32s
    (spk,_,privKeys) <- HTLCGenerators.offeredHTLC
    hashType <- CryptoGenerators.hashType
    amount <- CurrencyUnitGenerator.satoshis
    witSPK = P2WSHWitnessSPKV0(spk)
    uScriptWitness = P2WSHWitnessV0(spk)
    output <- TransactionGenerators.output
    (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(spk,amount)
    input = TransactionInput(TransactionOutPoint(creditingTx.txId, outputIndex),EmptyScriptSignature,UInt32.zero)
    uWTx = WitnessTransaction(tc.validLockVersion,Seq(input),Seq(output),
      lockTime,TransactionWitness(Vector(uScriptWitness)))
    u = WitnessTxSigComponentRaw(uWTx,UInt32.zero,output,Policy.standardFlags)
    sig = TransactionSignatureCreator.createSig(u,privKeys(0),hashType)
    ss = OfferedHTLCScriptSig(sig,privKeys(0).publicKey)
    signedScriptWitness = P2WSHWitnessV0(spk, ss)
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt,signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version,oldTx.inputs,oldTx.outputs,oldTx.lockTime,txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,u.inputIndex,output,u.flags)
  } yield (txWitness,signedWtxSigComponent,privKeys)

  def signedOfferedHTLCPaymentWitness: Gen[WitComponent] = for {
    lockTime <- NumberGenerator.uInt32s
    (spk,hashPreImage,privKeys) <- HTLCGenerators.offeredHTLC
    hashType <- CryptoGenerators.hashType
    amount <- CurrencyUnitGenerator.satoshis
    witSPK = P2WSHWitnessSPKV0(spk)
    uScriptWitness = P2WSHWitnessV0(spk)
    output <- TransactionGenerators.output
    (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(spk,amount)
    input = TransactionInput(TransactionOutPoint(creditingTx.txId, outputIndex),EmptyScriptSignature,UInt32.zero)
    uWTx = WitnessTransaction(tc.validLockVersion,Seq(input),Seq(output),
      lockTime,TransactionWitness(Vector(uScriptWitness)))
    u = WitnessTxSigComponentRaw(uWTx,UInt32.zero,output,Policy.standardFlags)
    sig = TransactionSignatureCreator.createSig(u,privKeys(1),hashType)
    ss = OfferedHTLCScriptSig(hashPreImage,sig)
    signedScriptWitness = P2WSHWitnessV0(spk, ss)
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt,signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version,oldTx.inputs,oldTx.outputs,oldTx.lockTime,txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,u.inputIndex,output,u.flags)
  } yield (txWitness,signedWtxSigComponent,privKeys)

  //TODO: Add 'To me via HTLC-timeout transaction (timelocked)' for offered htlc

  def signedRefundHTLCDelay: Gen[WitComponent] = for {
    (scriptNum,sequence) <- TransactionGenerators.spendableCSVValues
    (spk,privKeys) <- HTLCGenerators.refundHTLC(scriptNum)
    hashType <- CryptoGenerators.hashType
    amount <- CurrencyUnitGenerator.satoshis
    witSPK = P2WSHWitnessSPKV0(spk)
    uScriptWitness = P2WSHWitnessV0(spk)
    output <- TransactionGenerators.output
    (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(spk,amount)
    input = TransactionInput(TransactionOutPoint(creditingTx.txId, outputIndex),EmptyScriptSignature,sequence)
    uWTx = WitnessTransaction(tc.validLockVersion,Seq(input),Seq(output),
      tc.lockTime,TransactionWitness(Vector(uScriptWitness)))
    u = WitnessTxSigComponentRaw(uWTx,UInt32.zero,output,Policy.standardFlags)
    sig = TransactionSignatureCreator.createSig(u,privKeys(1),hashType)
    ss = RefundHTLCScriptSig.createDelay(sig)
    signedScriptWitness = P2WSHWitnessV0(spk, ss)
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt,signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version,oldTx.inputs,oldTx.outputs,oldTx.lockTime,txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,u.inputIndex,output,u.flags)
  } yield (txWitness,signedWtxSigComponent,privKeys)

  def signedRefundHTLCRevocation: Gen[WitComponent] = for {
    scriptNum <- NumberGenerator.scriptNumbers
    sequence <- NumberGenerator.uInt32s
    (spk,privKeys) <- HTLCGenerators.refundHTLC(scriptNum)
    hashType <- CryptoGenerators.hashType
    amount <- CurrencyUnitGenerator.satoshis
    witSPK = P2WSHWitnessSPKV0(spk)
    uScriptWitness = P2WSHWitnessV0(spk)
    output <- TransactionGenerators.output
    (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(spk,amount)
    input = TransactionInput(TransactionOutPoint(creditingTx.txId, outputIndex),EmptyScriptSignature,sequence)
    uWTx = WitnessTransaction(tc.validLockVersion,Seq(input),Seq(output),
      tc.lockTime,TransactionWitness(Vector(uScriptWitness)))
    u = WitnessTxSigComponentRaw(uWTx,UInt32.zero,output,Policy.standardFlags)
    sig = TransactionSignatureCreator.createSig(u,privKeys(0),hashType)
    ss = RefundHTLCScriptSig.createRevocation(sig)
    signedScriptWitness = P2WSHWitnessV0(spk, ss)
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt,signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version,oldTx.inputs,oldTx.outputs,oldTx.lockTime,txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,u.inputIndex,output,u.flags)
  } yield (txWitness,signedWtxSigComponent,privKeys)
}

object HTLCWitnessGenerators extends HTLCWitnessGenerators
