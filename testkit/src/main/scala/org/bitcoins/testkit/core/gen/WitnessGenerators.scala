package org.bitcoins.testkit.core.gen

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.Gen

/**
  * Created by chris on 11/28/16.
  */
sealed abstract class WitnessGenerators extends BitcoinSLogger {

  /** Generates a random [[org.bitcoins.core.protocol.script.ScriptWitness]] */
  def scriptWitness: Gen[ScriptWitness] = {

    //TODO: I need to come back and uncomment out this code after fixing
    //#111 on the issue tracker. We should be able to support an arbtirary byte vector,
    //not only pre-defined script witness types

    //0 include here to generate the EmptyScriptWitness

    /*    val stack: Gen[Seq[scodec.bits.ByteVector]] = Gen.choose(0,10).flatMap(n => Gen.listOfN(n, NumberGenerator.bytes))
    stack.map { s: Seq[scodec.bits.ByteVector] =>
      val spkBytes = if (s.nonEmpty) s.head else Nil
      val cmpctSPK = CompactSizeUInt(UInt64(spkBytes.size))
      val scriptSigBytes: scodec.bits.ByteVector = if (s.size > 1) s.tail.flatten else Nil
      val cmpctScriptSig = CompactSizeUInt(UInt64(scriptSigBytes.size))

      val scriptSig = if (scriptSigBytes.isEmpty) EmptyScriptSignature else NonStandardScriptSignature(cmpctScriptSig.bytes ++ scriptSigBytes)
      val spk = if (spkBytes.isEmpty) EmptyScriptPubKey else NonStandardScriptPubKey(cmpctSPK.bytes ++ spkBytes)
      P2WSHWitnessV0(spk,scriptSig)
    }*/
    Gen.oneOf(p2wpkhWitnessV0, p2wshWitnessV0)
  }

  /** Generates a [[org.bitcoins.core.protocol.transaction.TransactionWitness]] with
    * the specified number of witnesses */
  def transactionWitness(numWitnesses: Int): Gen[TransactionWitness] =
    for {
      inputWitnesses <- Gen.listOfN(numWitnesses, Gen.option(scriptWitness))
    } yield TransactionWitness.fromWitOpt(inputWitnesses.toVector)

  def transactionWitness: Gen[TransactionWitness] =
    for {
      num <- Gen.choose(1, 10)
      wit <- transactionWitness(num)
    } yield wit

  /** Generates a validly signed [[org.bitcoins.core.protocol.transaction.TransactionWitness]] */
  def signedP2WPKHTransactionWitness: Gen[
    (TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] =
    for {
      privKey <- CryptoGenerators.privateKey
      amount <- CurrencyUnitGenerator.satoshis
      hashType <- CryptoGenerators.hashType
      witScriptPubKey = P2WPKHWitnessSPKV0(privKey.publicKey)
      unsignedScriptWitness = P2WPKHWitnessV0(privKey.publicKey)
      unsignedWTxSigComponent = createUnsignedRawWTxSigComponent(
        witScriptPubKey,
        amount,
        unsignedScriptWitness,
        None)
      createdSig = TransactionSignatureCreator.createSig(
        unsignedWTxSigComponent,
        privKey,
        hashType)
      scriptWitness = P2WPKHWitnessV0(privKey.publicKey, createdSig)

    } yield {
      val (witness, signedWtxSigComponent) =
        createSignedWTxComponent(scriptWitness, unsignedWTxSigComponent)
      (witness, signedWtxSigComponent, Seq(privKey))
    }

  def signedP2WSHP2PKTransactionWitness: Gen[
    (TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] =
    for {
      (scriptPubKey, privKeys) <- ScriptGenerators.p2pkScriptPubKey
      amount <- CurrencyUnitGenerator.satoshis
      hashType <- CryptoGenerators.hashType
      witScriptPubKey = P2WSHWitnessSPKV0(scriptPubKey)
      unsignedScriptWitness = P2WSHWitnessV0(scriptPubKey)
      u = createUnsignedRawWTxSigComponent(witScriptPubKey,
                                           amount,
                                           unsignedScriptWitness,
                                           None)
      createdSig = TransactionSignatureCreator.createSig(u, privKeys, hashType)
      signedScriptWitness = P2WSHWitnessV0(scriptPubKey,
                                           P2PKScriptSignature(createdSig))
      oldTx = u.transaction
      txWitness = TransactionWitness(
        oldTx.witness.witnesses
          .updated(u.inputIndex.toInt, signedScriptWitness))
      wtx = WitnessTransaction(oldTx.version,
                               oldTx.inputs,
                               oldTx.outputs,
                               oldTx.lockTime,
                               txWitness)
      signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,
                                                       u.inputIndex,
                                                       u.output,
                                                       u.flags)
    } yield (txWitness, signedWtxSigComponent, Seq(privKeys))

  def signedP2WSHP2PKHTransactionWitness: Gen[
    (TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] =
    for {
      (scriptPubKey, privKey) <- ScriptGenerators.p2pkhScriptPubKey
      amount <- CurrencyUnitGenerator.satoshis
      hashType <- CryptoGenerators.hashType
      witScriptPubKey = P2WSHWitnessSPKV0(scriptPubKey)
      unsignedScriptWitness = P2WSHWitnessV0(scriptPubKey)
      u = createUnsignedRawWTxSigComponent(witScriptPubKey,
                                           amount,
                                           unsignedScriptWitness,
                                           None)
      createdSig = TransactionSignatureCreator.createSig(u, privKey, hashType)
      signedScriptWitness = P2WSHWitnessV0(
        scriptPubKey,
        P2PKHScriptSignature(createdSig, privKey.publicKey))
      oldTx = u.transaction
      txWitness = TransactionWitness(
        oldTx.witness.witnesses
          .updated(u.inputIndex.toInt, signedScriptWitness))
      wtx = WitnessTransaction(oldTx.version,
                               oldTx.inputs,
                               oldTx.outputs,
                               oldTx.lockTime,
                               txWitness)
      signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,
                                                       u.inputIndex,
                                                       u.output,
                                                       u.flags)
    } yield (txWitness, signedWtxSigComponent, Seq(privKey))

  def signedP2WSHMultiSigTransactionWitness: Gen[
    (TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] =
    for {
      (scriptPubKey, privKeys) <- ScriptGenerators.multiSigScriptPubKey
      amount <- CurrencyUnitGenerator.satoshis
      hashType <- CryptoGenerators.hashType
      witScriptPubKey = P2WSHWitnessSPKV0(scriptPubKey)
      unsignedScriptWitness = P2WSHWitnessV0(scriptPubKey)
      u = createUnsignedRawWTxSigComponent(witScriptPubKey,
                                           amount,
                                           unsignedScriptWitness,
                                           None)
      signedScriptSig = multiSigScriptSigGenHelper(privKeys,
                                                   scriptPubKey,
                                                   u,
                                                   hashType)
      signedScriptWitness = P2WSHWitnessV0(scriptPubKey, signedScriptSig)
      oldTx = u.transaction
      txWitness = TransactionWitness(
        oldTx.witness.witnesses
          .updated(u.inputIndex.toInt, signedScriptWitness))
      wtx = WitnessTransaction(oldTx.version,
                               oldTx.inputs,
                               oldTx.outputs,
                               oldTx.lockTime,
                               txWitness)
      signedWtxSigComponent = WitnessTxSigComponentRaw(wtx,
                                                       u.inputIndex,
                                                       u.output,
                                                       u.flags)
    } yield (txWitness, signedWtxSigComponent, privKeys)

  /**
    * Generates a random signed [[org.bitcoins.core.protocol.transaction.TransactionWitness TransactionWitness]]
    * with the corresponding [[org.bitcoins.core.crypto.WitnessTxSigComponent WitnessTxSigComponent]]
    * and [[org.bitcoins.core.crypto.ECPrivateKey ECPrivateKey]]
    */
  def signedP2WSHTransactionWitness: Gen[
    (TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedP2WSHP2PKTransactionWitness,
              signedP2WSHP2PKHTransactionWitness,
              signedP2WSHMultiSigTransactionWitness)
  }

  /** Helps generate a signed
    * [[org.bitcoins.core.protocol.script.MultiSignatureScriptSignature MultiSignatureScriptSignature]] */
  private def multiSigScriptSigGenHelper(
      privateKeys: Seq[ECPrivateKey],
      scriptPubKey: MultiSignatureScriptPubKey,
      unsignedWtxSigComponent: WitnessTxSigComponent,
      hashType: HashType): MultiSignatureScriptSignature = {
    val requiredSigs = scriptPubKey.requiredSigs
    val txSignatures = for {
      i <- 0 until requiredSigs
    } yield
      TransactionSignatureCreator.createSig(unsignedWtxSigComponent,
                                            privateKeys(i),
                                            hashType)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = MultiSignatureScriptSignature(txSignatures)
    signedScriptSig
  }

  /** Generates a random [[org.bitcoins.core.protocol.script.P2WPKHWitnessV0 P2PWPKHWitnessV0]] */
  def p2wpkhWitnessV0: Gen[P2WPKHWitnessV0] =
    for {
      publicKey <- CryptoGenerators.publicKey
      sig <- CryptoGenerators.digitalSignature
    } yield P2WPKHWitnessV0(publicKey, sig)

  /** Generates a random [[org.bitcoins.core.protocol.script.P2WPKHWitnessV0 P2PWPKHWitnessV0]] */
  def p2wshWitnessV0: Gen[P2WSHWitnessV0] =
    for {
      (redeem, _) <- ScriptGenerators.scriptPubKey
      scriptSig <- ScriptGenerators.scriptSignature
    } yield P2WSHWitnessV0(redeem, scriptSig)

  /** Takes a signed [[org.bitcoins.core.protocol.script.ScriptWitness ScriptWitness]] and an unsignedTx
    * and adds the witness to the unsigned
    * [[org.bitcoins.core.protocol.transaction.WitnessTransaction WitnessTransaction]] */
  def createSignedWTxComponent(
      witness: ScriptWitness,
      unsignedWTxComponent: WitnessTxSigComponent): (
      TransactionWitness,
      WitnessTxSigComponent) = {
    val signedTxWitness = TransactionWitness.fromWitOpt(Vector(Some(witness)))
    val unsignedSpendingTx = unsignedWTxComponent.transaction
    val signedSpendingTx = WitnessTransaction(unsignedSpendingTx.version,
                                              unsignedSpendingTx.inputs,
                                              unsignedSpendingTx.outputs,
                                              unsignedSpendingTx.lockTime,
                                              signedTxWitness)
    val signedWtxSigComponent = unsignedWTxComponent match {
      case wtxP2SH: WitnessTxSigComponentP2SH =>
        WitnessTxSigComponent(signedSpendingTx,
                              unsignedWTxComponent.inputIndex,
                              wtxP2SH.output,
                              unsignedWTxComponent.flags)
      case wtxRaw: WitnessTxSigComponentRaw =>
        WitnessTxSigComponent(signedSpendingTx,
                              unsignedWTxComponent.inputIndex,
                              wtxRaw.output,
                              unsignedWTxComponent.flags)
    }

    (signedTxWitness, signedWtxSigComponent)
  }

  /** Creates a unsigned [[org.bitcoins.core.crypto.WitnessTxSigComponent]] from
    * the given parameters */
  def createUnsignedRawWTxSigComponent(
      witScriptPubKey: WitnessScriptPubKey,
      amount: CurrencyUnit,
      unsignedScriptWitness: ScriptWitness,
      sequence: Option[UInt32]): WitnessTxSigComponentRaw = {
    val tc = TransactionConstants
    val flags = Policy.standardScriptVerifyFlags
    val witness =
      TransactionWitness.fromWitOpt(Vector(Some(unsignedScriptWitness)))
    val (creditingTx, outputIndex) =
      TransactionGenerators.buildCreditingTransaction(witScriptPubKey, amount)
    val (unsignedSpendingTx, inputIndex) =
      TransactionGenerators.buildSpendingTransaction(
        tc.validLockVersion,
        creditingTx,
        EmptyScriptSignature,
        outputIndex,
        tc.lockTime,
        sequence.getOrElse(tc.sequence),
        witness)
    val output = creditingTx.outputs(outputIndex.toInt)
    val unsignedWtxSigComponent =
      WitnessTxSigComponentRaw(unsignedSpendingTx, inputIndex, output, flags)
    unsignedWtxSigComponent
  }
}

object WitnessGenerators extends WitnessGenerators
