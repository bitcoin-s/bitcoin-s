package org.bitcoins.core.gen

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.EscrowTimeoutHelper
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

    /*    val stack: Gen[Seq[Seq[Byte]]] = Gen.choose(0,10).flatMap(n => Gen.listOfN(n, NumberGenerator.bytes))
    stack.map { s: Seq[Seq[Byte]] =>
      val spkBytes = if (s.nonEmpty) s.head else Nil
      val cmpctSPK = CompactSizeUInt(UInt64(spkBytes.size))
      val scriptSigBytes: Seq[Byte] = if (s.size > 1) s.tail.flatten else Nil
      val cmpctScriptSig = CompactSizeUInt(UInt64(scriptSigBytes.size))

      val scriptSig = if (scriptSigBytes.isEmpty) EmptyScriptSignature else NonStandardScriptSignature(cmpctScriptSig.bytes ++ scriptSigBytes)
      val spk = if (spkBytes.isEmpty) EmptyScriptPubKey else NonStandardScriptPubKey(cmpctSPK.bytes ++ spkBytes)
      P2WSHWitnessV0(spk,scriptSig)
    }*/
    Gen.oneOf(p2wpkhWitnessV0, p2wshWitnessV0)
  }

  /** Generates a [[TransactionWitness]] with the specified number of witnesses */
  def transactionWitness(numWitnesses: Int): Gen[TransactionWitness] = for {
    inputWitnesses <- Gen.listOfN(numWitnesses, Gen.option(scriptWitness))
  } yield TransactionWitness.fromWitOpt(inputWitnesses)

  def transactionWitness: Gen[TransactionWitness] = for {
    num <- Gen.choose(1, 10)
    wit <- transactionWitness(num)
  } yield wit

  /** Generates a validly signed [[TransactionWitness]] */
  def signedP2WPKHTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    privKey <- CryptoGenerators.privateKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = P2WPKHWitnessSPKV0(privKey.publicKey)
    unsignedScriptWitness = P2WPKHWitnessV0(privKey.publicKey)
    unsignedWTxSigComponent = createUnsignedRawWTxSigComponent(witScriptPubKey, amount,
      unsignedScriptWitness, None)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent, privKey, hashType)
    scriptWitness = P2WPKHWitnessV0(privKey.publicKey, createdSig)
    (witness, signedWtxSigComponent) = createSignedWTxComponent(scriptWitness, unsignedWTxSigComponent)
  } yield (witness, signedWtxSigComponent, Seq(privKey))

  def signedP2WSHP2PKTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = P2WSHWitnessSPKV0(scriptPubKey)
    unsignedScriptWitness = P2WSHWitnessV0(scriptPubKey)
    u = createUnsignedRawWTxSigComponent(witScriptPubKey, amount,
      unsignedScriptWitness, None)
    createdSig = TransactionSignatureCreator.createSig(u, privKeys, hashType)
    signedScriptWitness = P2WSHWitnessV0(scriptPubKey, P2PKScriptSignature(createdSig))
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt, signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version, oldTx.inputs, oldTx.outputs, oldTx.lockTime, txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx, u.inputIndex, witScriptPubKey, u.flags, u.amount)
  } yield (txWitness, signedWtxSigComponent, Seq(privKeys))

  def signedP2WSHP2PKHTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKey) <- ScriptGenerators.p2pkhScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = P2WSHWitnessSPKV0(scriptPubKey)
    unsignedScriptWitness = P2WSHWitnessV0(scriptPubKey)
    u = createUnsignedRawWTxSigComponent(witScriptPubKey, amount, unsignedScriptWitness, None)
    createdSig = TransactionSignatureCreator.createSig(u, privKey, hashType)
    signedScriptWitness = P2WSHWitnessV0(scriptPubKey, P2PKHScriptSignature(createdSig, privKey.publicKey))
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt, signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version, oldTx.inputs, oldTx.outputs, oldTx.lockTime, txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx, u.inputIndex, witScriptPubKey, u.flags, u.amount)
  } yield (txWitness, signedWtxSigComponent, Seq(privKey))

  def signedP2WSHMultiSigTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.multiSigScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = P2WSHWitnessSPKV0(scriptPubKey)
    unsignedScriptWitness = P2WSHWitnessV0(scriptPubKey)
    u = createUnsignedRawWTxSigComponent(witScriptPubKey, amount,
      unsignedScriptWitness, None)
    signedScriptSig = multiSigScriptSigGenHelper(privKeys, scriptPubKey, u, hashType)
    signedScriptWitness = P2WSHWitnessV0(scriptPubKey, signedScriptSig)
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt, signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version, oldTx.inputs, oldTx.outputs, oldTx.lockTime, txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx, u.inputIndex, witScriptPubKey, u.flags, u.amount)
  } yield (txWitness, signedWtxSigComponent, privKeys)

  /**
   * Generates a random signed [[TransactionWitness]] with the corresponding [[WitnessTxSigComponent]]
   * and [[ECPrivateKey]]s
   */
  def signedP2WSHTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedP2WSHP2PKTransactionWitness, signedP2WSHP2PKHTransactionWitness,
      signedP2WSHMultiSigTransactionWitness, signedP2WSHEscrowTimeoutWitness)
  }

  def signedP2WSHMultiSigEscrowTimeoutWitness: Gen[(TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.escrowTimeoutScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = P2WSHWitnessSPKV0(scriptPubKey)
    unsignedScriptWitness = P2WSHWitnessV0(scriptPubKey)
    u = createUnsignedRawWTxSigComponent(witScriptPubKey, amount,
      unsignedScriptWitness, None)
    signedScriptSig = csvEscrowTimeoutGenHelper(privKeys, scriptPubKey, u, hashType)
    witness = EscrowTimeoutHelper.buildEscrowTimeoutScriptWitness(signedScriptSig, scriptPubKey, u)
    oldTx = u.transaction
    wTx = WitnessTransaction(oldTx.version, oldTx.inputs, oldTx.outputs, oldTx.lockTime, witness)
    signedWTxSigComponent = WitnessTxSigComponentRaw(wTx, u.inputIndex, witScriptPubKey, u.flags, u.amount)
  } yield (witness, signedWTxSigComponent, privKeys)

  def spendableP2WSHTimeoutEscrowTimeoutWitness: Gen[(TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = for {
    (p2pkh, privKey) <- ScriptGenerators.p2pkhScriptPubKey
    (scriptNum, sequence) <- TransactionGenerators.spendableCSVValues
    csv = CSVScriptPubKey(scriptNum, p2pkh)
    (m, _) <- ScriptGenerators.smallMultiSigScriptPubKey
    scriptPubKey = EscrowTimeoutScriptPubKey(m, csv)
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = P2WSHWitnessSPKV0(scriptPubKey)
    unsignedScriptWitness = P2WSHWitnessV0(scriptPubKey)
    u = createUnsignedRawWTxSigComponent(
      witScriptPubKey,
      amount, unsignedScriptWitness, Some(sequence)
    )
    createdSig = TransactionSignatureCreator.createSig(u, privKey, hashType)
    scriptSig = CSVScriptSignature(P2PKHScriptSignature(createdSig, privKey.publicKey))
    signedScriptWitness = P2WSHWitnessV0(scriptPubKey, EscrowTimeoutScriptSignature.fromLockTime(scriptSig))
    //ScriptWitness(scriptPubKey.asm.flatMap(_.bytes) +: Seq(ScriptNumber.zero.bytes, privKey.publicKey.bytes,
    //createdSig.bytes))
    oldTx = u.transaction
    txWitness = TransactionWitness(oldTx.witness.witnesses.updated(u.inputIndex.toInt, signedScriptWitness))
    wtx = WitnessTransaction(oldTx.version, oldTx.inputs, oldTx.outputs, oldTx.lockTime, txWitness)
    signedWtxSigComponent = WitnessTxSigComponentRaw(wtx, u.inputIndex, witScriptPubKey, u.flags, u.amount)
  } yield (txWitness, signedWtxSigComponent, Seq(privKey))

  def signedP2WSHEscrowTimeoutWitness: Gen[(TransactionWitness, WitnessTxSigComponentRaw, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedP2WSHMultiSigEscrowTimeoutWitness, spendableP2WSHTimeoutEscrowTimeoutWitness)
  }

  /** Helps generate a signed [[MultiSignatureScriptSignature]] */
  private def multiSigScriptSigGenHelper(
    privateKeys:             Seq[ECPrivateKey],
    scriptPubKey:            MultiSignatureScriptPubKey,
    unsignedWtxSigComponent: WitnessTxSigComponent,
    hashType:                HashType
  ): MultiSignatureScriptSignature = {
    val requiredSigs = scriptPubKey.requiredSigs
    val txSignatures = for {
      i <- 0 until requiredSigs
    } yield TransactionSignatureCreator.createSig(unsignedWtxSigComponent, privateKeys(i), hashType)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = MultiSignatureScriptSignature(txSignatures)
    signedScriptSig
  }

  def csvEscrowTimeoutGenHelper(privateKeys: Seq[ECPrivateKey], scriptPubKey: EscrowTimeoutScriptPubKey,
                                unsignedWtxSigComponent: WitnessTxSigComponent,
                                hashType:                HashType): EscrowTimeoutScriptSignature = {
    if (scriptPubKey.escrow.requiredSigs == 0) {
      EscrowTimeoutScriptSignature.fromMultiSig(MultiSignatureScriptSignature(Nil))
    } else if (privateKeys.size == 1) {
      val signature = csvEscrowTimeoutGenSignature(privateKeys.head, scriptPubKey, unsignedWtxSigComponent, hashType)
      EscrowTimeoutScriptSignature.fromMultiSig(MultiSignatureScriptSignature(Seq(signature)))
    } else {
      val multiSig = multiSigScriptSigGenHelper(privateKeys, scriptPubKey.escrow, unsignedWtxSigComponent, hashType)
      EscrowTimeoutScriptSignature.fromMultiSig(multiSig)
    }
  }

  def csvEscrowTimeoutGenSignature(privKey: ECPrivateKey, scriptPubKey: EscrowTimeoutScriptPubKey,
                                   unsignedWtxSigComponent: WitnessTxSigComponent, hashType: HashType): ECDigitalSignature = {

    val signature = TransactionSignatureCreator.createSig(unsignedWtxSigComponent, privKey, hashType)
    signature
  }

  /** Generates a random [[org.bitcoins.core.protocol.script.P2WPKHWitnessV0]] */
  def p2wpkhWitnessV0: Gen[P2WPKHWitnessV0] = for {
    publicKey <- CryptoGenerators.publicKey
    sig <- CryptoGenerators.digitalSignature
  } yield P2WPKHWitnessV0(publicKey, sig)

  /** Generates a random [[org.bitcoins.core.protocol.script.P2WSHWitnessV0]] */
  def p2wshWitnessV0: Gen[P2WSHWitnessV0] = for {
    (redeem, _) <- ScriptGenerators.scriptPubKey
    scriptSig <- ScriptGenerators.scriptSignature
  } yield P2WSHWitnessV0(redeem, scriptSig)

  /** Takes a signed [[ScriptWitness]] and an unsignedTx and adds the witness to the unsigned [[WitnessTransaction]] */
  def createSignedWTxComponent(witness: ScriptWitness, unsignedWTxComponent: WitnessTxSigComponent): (TransactionWitness, WitnessTxSigComponent) = {
    val signedTxWitness = TransactionWitness.fromWitOpt(Seq(Some(witness)))
    val unsignedSpendingTx = unsignedWTxComponent.transaction
    val signedSpendingTx = WitnessTransaction(unsignedSpendingTx.version, unsignedSpendingTx.inputs, unsignedSpendingTx.outputs,
      unsignedSpendingTx.lockTime, signedTxWitness)
    val signedWtxSigComponent = unsignedWTxComponent match {
      case wtxP2SH: WitnessTxSigComponentP2SH =>
        WitnessTxSigComponent(signedSpendingTx, unsignedWTxComponent.inputIndex,
          wtxP2SH.scriptPubKey, unsignedWTxComponent.flags, unsignedWTxComponent.amount)
      case wtxRaw: WitnessTxSigComponentRaw =>
        WitnessTxSigComponent(signedSpendingTx, unsignedWTxComponent.inputIndex,
          wtxRaw.scriptPubKey, unsignedWTxComponent.flags, unsignedWTxComponent.amount)
    }

    (signedTxWitness, signedWtxSigComponent)
  }

  /** Creates a unsigned [[WitnessTxSigComponent]] from the given parameters */
  def createUnsignedRawWTxSigComponent(witScriptPubKey: WitnessScriptPubKey, amount: CurrencyUnit,
                                       unsignedScriptWitness: ScriptWitness, sequence: Option[UInt32]): WitnessTxSigComponentRaw = {
    val tc = TransactionConstants
    val flags = Policy.standardScriptVerifyFlags
    val witness = TransactionWitness.fromWitOpt(Seq(Some(unsignedScriptWitness)))
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(witScriptPubKey, amount)
    val (unsignedSpendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(tc.validLockVersion, creditingTx,
      EmptyScriptSignature, outputIndex, tc.lockTime,
      sequence.getOrElse(tc.sequence), witness)
    val unsignedWtxSigComponent = WitnessTxSigComponentRaw(unsignedSpendingTx, inputIndex, witScriptPubKey, flags, amount)
    unsignedWtxSigComponent
  }
}

object WitnessGenerators extends WitnessGenerators
