package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{ECPrivateKey, Sha256Digest, WitnessTxSigComponentRaw}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.constant.{OP_0, OP_1, ScriptConstant, ScriptNumber}
import org.bitcoins.core.util.{BitcoinScriptUtil, CryptoUtil}
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.scalacheck.Gen

sealed abstract class HTLCGenerators {
  def refundHTLC: Gen[(RefundHTLC, Seq[ECPrivateKey])] = for {
    locktime <- NumberGenerator.scriptNumbers
    htlc <- refundHTLC(locktime)
  } yield htlc

  def refundHTLC(scriptNum: ScriptNumber): Gen[(RefundHTLC, Seq[ECPrivateKey])] = for {
    revocationKey <- CryptoGenerators.privateKey
    delayedKey <- CryptoGenerators.privateKey
    refund = RefundHTLC(revocationKey.publicKey,scriptNum,delayedKey.publicKey)
    privKeys = Seq(revocationKey,delayedKey)
  } yield (refund,privKeys)

  def offeredHTLC: Gen[(OfferedHTLC, Sha256Digest, Seq[ECPrivateKey])] = for {
    revocationKey <- CryptoGenerators.privateKey
    remoteKey <- CryptoGenerators.privateKey
    localKey <- CryptoGenerators.privateKey
    paymentPreImage <- CryptoGenerators.sha256Digest
    paymentHash = CryptoUtil.sha256Hash160(paymentPreImage.bytes)
    htlc = OfferedHTLC(revocationKey.publicKey,remoteKey.publicKey,localKey.publicKey,paymentHash)
    keys = Seq(revocationKey,remoteKey,localKey)
  } yield (htlc,paymentPreImage,keys)

  def receivedHTLC: Gen[(ReceivedHTLC,Seq[ECPrivateKey])] = for {
    lockTime <- NumberGenerator.scriptNumbers
    htlc <- receivedHTLC(lockTime)
  } yield htlc

  def receivedHTLC(lockTime: ScriptNumber): Gen[(ReceivedHTLC,Seq[ECPrivateKey])] = for {
    revocationKey <- CryptoGenerators.privateKey
    remoteKey <- CryptoGenerators.privateKey
    localKey <- CryptoGenerators.privateKey
    paymentHash <- CryptoGenerators.ripeMd160Digest
    htlc = ReceivedHTLC(revocationKey.publicKey,remoteKey.publicKey,paymentHash,localKey.publicKey,lockTime)
    keys = Seq(revocationKey,remoteKey,localKey)
  } yield (htlc,keys)

  def refundHTLCScriptSig: Gen[RefundHTLCScriptSig] = for {
    bool <- Gen.oneOf(OP_0,OP_1)
    sig <- CryptoGenerators.digitalSignature
    sigConst = ScriptConstant(sig.bytes)
    asm = BitcoinScriptUtil.calculatePushOp(sigConst) ++ Seq(sigConst, bool)
  } yield RefundHTLCScriptSig.fromAsm(asm)

  def offeredHTLCScriptSig: Gen[OfferedHTLCScriptSig] = Gen.oneOf(offeredHTLCScriptSigRevocation,
    offeredHTLCScriptSigPayment)

  def offeredHTLCScriptSigRevocation: Gen[OfferedHTLCScriptSig] = for {
    key <- CryptoGenerators.publicKey
    sig <- CryptoGenerators.digitalSignature
  } yield OfferedHTLCScriptSig(sig,key)

  def offeredHTLCScriptSigPayment: Gen[OfferedHTLCScriptSig] = for {
    hash <- CryptoGenerators.sha256Digest
    sig <- CryptoGenerators.digitalSignature
  } yield OfferedHTLCScriptSig(hash,sig)

  def receivedHTLCScriptSigTimeout: Gen[ReceivedHTLCScriptSig] = for {
    sig <- CryptoGenerators.digitalSignature
  } yield ReceivedHTLCScriptSig(sig)

  def receivedHTLCScriptSigRevocation: Gen[ReceivedHTLCScriptSig] = for {
    key <- CryptoGenerators.publicKey
    sig <- CryptoGenerators.digitalSignature
  } yield ReceivedHTLCScriptSig(sig,key)

  def receivedHTLCScriptSig: Gen[ReceivedHTLCScriptSig] = Gen.oneOf(receivedHTLCScriptSigTimeout,
    receivedHTLCScriptSigRevocation)

  def signedReceivedHTLCTimeoutTx: Gen[(WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (_,wTxSigComponent,privKeys) <- HTLCWitnessGenerators.signedReceivedHTLCTimeoutWitness
  } yield (wTxSigComponent,privKeys)

  def signedReceivedHTLCRevokedTx: Gen[(WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (_,wTxSigComponent, privKeys) <- HTLCWitnessGenerators.signedReceivedHTLCRevokedWitness
  } yield (wTxSigComponent,privKeys)

  def signedReceivedHTLCTx: Gen[(WitnessTxSigComponentRaw,Seq[ECPrivateKey])] = {
    Gen.oneOf(signedReceivedHTLCRevokedTx, signedReceivedHTLCTimeoutTx)
  }

  def signedOfferedHTLCRevokedTx: Gen[(WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (_,wTxSigComponent,privKeys) <- HTLCWitnessGenerators.signedOfferedHTLCRevocationWitness
  } yield (wTxSigComponent,privKeys)

  def signedOfferedHTLCPaymentTx: Gen[(WitnessTxSigComponentRaw,Seq[ECPrivateKey])] = for {
    (_,wTxSigComponent,privKeys) <- HTLCWitnessGenerators.signedOfferedHTLCPaymentWitness
  } yield (wTxSigComponent,privKeys)

  def signedOfferedHTLCTx: Gen[(WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedOfferedHTLCRevokedTx,
      signedOfferedHTLCPaymentTx)
  }

  def signedRefundHTLCDelayTx: Gen[(WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (_,wTxSigComponent, privKeys) <- HTLCWitnessGenerators.signedRefundHTLCDelay
  } yield (wTxSigComponent, privKeys)

  def signedRefundHTLCRevocationTx: Gen[(WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (_,wTxSigComponent, privKeys) <- HTLCWitnessGenerators.signedRefundHTLCRevocation
  } yield (wTxSigComponent,privKeys)

  def signedRefundHTLCTx: Gen[(WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = Gen.oneOf(signedRefundHTLCDelayTx,
    signedRefundHTLCRevocationTx)

}

object HTLCGenerators extends HTLCGenerators
