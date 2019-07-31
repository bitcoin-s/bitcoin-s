package org.bitcoins.testkit.core.gen

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.crypto.{TransactionSignatureCreator, _}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{P2SHScriptPubKey, _}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.signer.{MultiSigSigner, P2PKHSigner, P2PKSigner}
import org.scalacheck.Gen

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

//TODO: Need to provide generators for [[NonStandardScriptSignature]] and [[NonStandardScriptPubKey]]
sealed abstract class ScriptGenerators extends BitcoinSLogger {
  val timeout = 5.seconds

  def p2pkScriptSignature: Gen[P2PKScriptSignature] =
    for {
      digitalSignature <- CryptoGenerators.digitalSignature
    } yield P2PKScriptSignature(digitalSignature)

  def p2pkhScriptSignature: Gen[P2PKHScriptSignature] =
    for {
      privKey <- CryptoGenerators.privateKey
      hash <- CryptoGenerators.doubleSha256Digest
      signature = privKey.sign(hash)
    } yield P2PKHScriptSignature(signature, privKey.publicKey)

  def multiSignatureScriptSignature: Gen[MultiSignatureScriptSignature] = {
    val signatures: Gen[Seq[ECDigitalSignature]] = for {
      numKeys <- Gen.choose(1, Consensus.maxPublicKeysPerMultiSig)
      hash <- CryptoGenerators.doubleSha256Digest
    } yield
      for {
        _ <- 0 until numKeys
        privKey = ECPrivateKey()
      } yield privKey.sign(hash)
    signatures.map(sigs => MultiSignatureScriptSignature(sigs))
  }

  def emptyScriptSignature = p2pkhScriptSignature.map(_ => EmptyScriptSignature)

  /**
    * Generates a P2SH script signature
    *
    * @note the redeem script and the script signature DO NOT evaluate to true
    * if executed by [[org.bitcoins.core.script.interpreter.ScriptInterpreter]]
    */
  def p2shScriptSignature: Gen[P2SHScriptSignature] =
    for {
      (scriptPubKey, _) <- randomNonP2SHScriptPubKey
      scriptSig <- pickCorrespondingScriptSignature(scriptPubKey)
      p2shScriptSig = P2SHScriptSignature(scriptSig, scriptPubKey)
    } yield p2shScriptSig

  def cltvScriptSignature: Gen[CLTVScriptSignature] =
    for {
      scriptSig <- randomNonLockTimeScriptSig
    } yield CLTVScriptSignature(scriptSig)

  def csvScriptSignature: Gen[CSVScriptSignature] =
    for {
      scriptSig <- randomNonLockTimeScriptSig
    } yield CSVScriptSignature(scriptSig)

  def p2pkScriptPubKey: Gen[(P2PKScriptPubKey, ECPrivateKey)] =
    for {
      privKey <- CryptoGenerators.privateKey
      pubKey = privKey.publicKey
      p2pk = P2PKScriptPubKey(pubKey)
    } yield (p2pk, privKey)

  def p2pkhScriptPubKey: Gen[(P2PKHScriptPubKey, ECPrivateKey)] =
    for {
      privKey <- CryptoGenerators.privateKey
      pubKey = privKey.publicKey
      p2pkh = P2PKHScriptPubKey(pubKey)
    } yield (p2pkh, privKey)

  def cltvScriptPubKey: Gen[(CLTVScriptPubKey, Seq[ECPrivateKey])] =
    for {
      num <- NumberGenerator.scriptNumbers
      (cltv, privKeys, num) <- cltvScriptPubKey(num)
    } yield (cltv, privKeys)

  def cltvScriptPubKey(num: ScriptNumber): Gen[
    (CLTVScriptPubKey, Seq[ECPrivateKey], ScriptNumber)] =
    for {
      (scriptPubKey, privKeys) <- randomNonLockTimeNonP2SHScriptPubKey
    } yield {
      val cltv = CLTVScriptPubKey(num, scriptPubKey)
      (cltv, privKeys, num)
    }

  def csvScriptPubKey(num: ScriptNumber): Gen[
    (CSVScriptPubKey, Seq[ECPrivateKey], ScriptNumber)] =
    for {
      (scriptPubKey, privKeys) <- randomNonLockTimeNonP2SHScriptPubKey
    } yield {
      val csv = CSVScriptPubKey(num, scriptPubKey)
      (csv, privKeys, num)
    }

  def csvScriptPubKey: Gen[(CSVScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (scriptPubKey, privKeys) <- randomNonLockTimeNonP2SHScriptPubKey
      num <- NumberGenerator.scriptNumbers
      csv = CSVScriptPubKey(num, scriptPubKey)
    } yield (csv, privKeys)

  def multiSigScriptPubKey: Gen[
    (MultiSignatureScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (privateKeys, requiredSigs) <- CryptoGenerators.privateKeySeqWithRequiredSigs
      pubKeys = privateKeys.map(_.publicKey)
      multiSignatureScriptPubKey = MultiSignatureScriptPubKey(requiredSigs,
                                                              pubKeys)
    } yield (multiSignatureScriptPubKey, privateKeys)

  def smallMultiSigScriptPubKey: Gen[
    (MultiSignatureScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (privateKeys, requiredSigs) <- CryptoGenerators.smallPrivateKeySeqWithRequiredSigs
      pubKeys = privateKeys.map(_.publicKey)
      multiSignatureScriptPubKey = MultiSignatureScriptPubKey(requiredSigs,
                                                              pubKeys)
    } yield (multiSignatureScriptPubKey, privateKeys)

  def p2shScriptPubKey: Gen[(P2SHScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (randomScriptPubKey, privKeys) <- randomNonP2SHScriptPubKey
      p2sh = P2SHScriptPubKey(randomScriptPubKey)
    } yield (p2sh, privKeys)

  def emptyScriptPubKey: Gen[(ScriptPubKey, Seq[ECPrivateKey])] =
    (EmptyScriptPubKey, Nil)

  /** Creates a basic version 0 P2WPKH scriptpubkey */
  def p2wpkhSPKV0: Gen[(P2WPKHWitnessSPKV0, Seq[ECPrivateKey])] =
    for {
      privKey <- CryptoGenerators.privateKey
    } yield (P2WPKHWitnessSPKV0(privKey.publicKey), Seq(privKey))

  def p2wshSPKV0: Gen[(P2WSHWitnessSPKV0, Seq[ECPrivateKey])] =
    randomNonP2SHScriptPubKey.map { spk =>
      (P2WSHWitnessSPKV0(spk._1), spk._2)
    }

  def witnessScriptPubKeyV0: Gen[(WitnessScriptPubKeyV0, Seq[ECPrivateKey])] =
    Gen.oneOf(p2wpkhSPKV0, p2wshSPKV0)

  /**
    * Creates an unassigned witness scriptPubKey.
    * Currently this is any witness script pubkey besides
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 WitnessScriptPubKeyV0]]
    */
  def unassignedWitnessScriptPubKey: Gen[
    (UnassignedWitnessScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (witV0, privKeys) <- p2wpkhSPKV0
      version <- Gen.oneOf(WitnessScriptPubKey.unassignedWitVersions)
      unassignedAsm = version +: witV0.asm.tail
    } yield (UnassignedWitnessScriptPubKey(unassignedAsm), privKeys)

  /** Generates an arbitrary `WitnessScriptPubKey` */
  def witnessScriptPubKey: Gen[(WitnessScriptPubKey, Seq[ECPrivateKey])] =
    Gen.oneOf(assignedWitnessScriptPubKey, unassignedWitnessScriptPubKey)

  def assignedWitnessScriptPubKey: Gen[
    (WitnessScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(p2wpkhSPKV0, p2wshSPKV0)
  }

  def witnessCommitment: Gen[(WitnessCommitment, Seq[ECPrivateKey])] =
    for {
      hash <- CryptoGenerators.doubleSha256Digest
    } yield (WitnessCommitment(hash), Nil)

  def randomNonP2SHScriptPubKey: Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(
      p2pkScriptPubKey.map(privKeyToSeq(_)),
      p2pkhScriptPubKey.map(privKeyToSeq(_)),
      cltvScriptPubKey.suchThat(
        !_._1.nestedScriptPubKey.isInstanceOf[CSVScriptPubKey]),
      csvScriptPubKey.suchThat(
        !_._1.nestedScriptPubKey.isInstanceOf[CLTVScriptPubKey]),
      multiSigScriptPubKey,
      p2wpkhSPKV0,
      unassignedWitnessScriptPubKey
    )
  }

  /**
    * This is used for creating time locked scriptPubKeys, we cannot nest CSV/CLTV/P2SH/Witness
    * ScriptPubKeys inside of timelock scriptPubKeys
    */
  def randomNonLockTimeNonP2SHScriptPubKey: Gen[
    (ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(p2pkScriptPubKey.map(privKeyToSeq(_)),
              p2pkhScriptPubKey.map(privKeyToSeq(_)),
              multiSigScriptPubKey)
  }

  def randomNonLockTimeScriptSig: Gen[ScriptSignature] = {
    Gen.oneOf(p2pkScriptSignature,
              p2pkhScriptSignature,
              multiSignatureScriptSignature,
              emptyScriptSignature,
              p2shScriptSignature)
  }

  def lockTimeScriptPubKey: Gen[(LockTimeScriptPubKey, Seq[ECPrivateKey])] =
    Gen.oneOf(cltvScriptPubKey, csvScriptPubKey)

  def lockTimeScriptSig: Gen[LockTimeScriptSignature] =
    Gen.oneOf(csvScriptSignature, cltvScriptSignature)

  /** Generates an arbitrary `ScriptPubKey` */
  def scriptPubKey: Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(
      p2pkScriptPubKey.map(privKeyToSeq(_)),
      p2pkhScriptPubKey.map(privKeyToSeq(_)),
      multiSigScriptPubKey,
      emptyScriptPubKey,
      cltvScriptPubKey,
      csvScriptPubKey,
      p2wpkhSPKV0,
      p2wshSPKV0,
      unassignedWitnessScriptPubKey,
      p2shScriptPubKey,
      witnessCommitment
    )
  }

  /** Generates an arbitrary `ScriptSignature` */
  def scriptSignature: Gen[ScriptSignature] = {
    Gen.oneOf(
      p2pkScriptSignature,
      p2pkhScriptSignature,
      multiSignatureScriptSignature,
      emptyScriptSignature,
      p2shScriptSignature
      //NOTE: This are commented out because it fail serializatoin symmetry
      //sicne we cannot properly type CSV/CLTV ScriptSigs w/o it's SPK
      //csvScriptSignature, cltvScriptSignature
    )
  }

  /**
    * Generates a `ScriptSignature` corresponding to the type of
    * `ScriptPubKey` given.
    * Note: Does NOT generate a correct/valid signature
    */
  private def pickCorrespondingScriptSignature(
      scriptPubKey: ScriptPubKey): Gen[ScriptSignature] = scriptPubKey match {
    case _: P2PKScriptPubKey           => p2pkScriptSignature
    case _: P2PKHScriptPubKey          => p2pkhScriptSignature
    case _: MultiSignatureScriptPubKey => multiSignatureScriptSignature
    case EmptyScriptPubKey             => emptyScriptSignature
    case _: CLTVScriptPubKey           => cltvScriptSignature
    case _: CSVScriptPubKey            => csvScriptSignature
    case _: WitnessScriptPubKeyV0 | _: UnassignedWitnessScriptPubKey =>
      emptyScriptSignature
    case x @ (_: P2SHScriptPubKey | _: NonStandardScriptPubKey |
        _: WitnessCommitment) =>
      throw new IllegalArgumentException(
        "Cannot pick for p2sh script pubkey, " +
          "non standard script pubkey or witness commitment got: " + x)
  }

  /**
    * Generates a signed `P2PKScriptSignature` that spends the
    * `P2PKScriptPubKey` correctly
    *
    * @return the signed `P2PKScriptSignature`,
    *         the `P2PKScriptPubKey` it spends, and the
    *         `ECPrivateKey` used to sign the scriptSig
    */
  def signedP2PKScriptSignature: Gen[
    (P2PKScriptSignature, P2PKScriptPubKey, ECPrivateKey)] =
    for {
      privateKey <- CryptoGenerators.privateKey
      hashType <- CryptoGenerators.hashType
      publicKey = privateKey.publicKey
      scriptPubKey = P2PKScriptPubKey(publicKey)
      (creditingTx, outputIndex) = TransactionGenerators
        .buildCreditingTransaction(scriptPubKey)
      scriptSig = P2PKScriptSignature(EmptyDigitalSignature)
      (spendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(
        creditingTx,
        scriptSig,
        outputIndex)
      txSigComponentFuture = P2PKSigner.sign(
        Seq(privateKey),
        creditingTx.outputs(outputIndex.toInt),
        spendingTx,
        inputIndex,
        hashType,
        false)
      txSigComponent = Await.result(txSigComponentFuture, timeout)
      //add the signature to the scriptSig instead of having an empty scriptSig
      signedScriptSig = txSigComponent.scriptSignature
        .asInstanceOf[P2PKScriptSignature]
    } yield (signedScriptSig, scriptPubKey, privateKey)

  /**
    * Generates a signed `P2PKHScriptSignature` that
    * spends the `P2PKHScriptPubKey` correctly
    *
    * @return the signed `P2PKHScriptSignature`, the
    *         `P2PKHScriptPubKey` it spends, and the
    *         `ECPrivateKey` used to sign the scriptSig
    */
  def signedP2PKHScriptSignature: Gen[
    (P2PKHScriptSignature, P2PKHScriptPubKey, ECPrivateKey)] =
    for {
      privateKey <- CryptoGenerators.privateKey
      hashType <- CryptoGenerators.hashType
      publicKey = privateKey.publicKey
      scriptPubKey = P2PKHScriptPubKey(publicKey)
      (creditingTx, outputIndex) = TransactionGenerators
        .buildCreditingTransaction(scriptPubKey)
      (unsignedTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(
        creditingTx,
        EmptyScriptSignature,
        outputIndex)
      txSigComponentFuture = P2PKHSigner.sign(
        Seq(privateKey),
        creditingTx.outputs(outputIndex.toInt),
        unsignedTx,
        inputIndex,
        hashType,
        false)
      txSigComponent = Await.result(txSigComponentFuture, timeout)
      signedScriptSig = txSigComponent.scriptSignature
        .asInstanceOf[P2PKHScriptSignature]
    } yield (signedScriptSig, scriptPubKey, privateKey)

  /**
    * Generates a signed
    * `MultiSignatureScriptSignature` that spends the
    * `MultiSignatureScriptPubKey` correctly
    * ti
    * @return the signed `MultiSignatureScriptSignature`,
    *         the `MultiSignatureScriptPubKey` it spends and the
    *         sequence of `ECPrivateKey` used to sign the scriptSig
    */
  def signedMultiSignatureScriptSignature: Gen[(
      MultiSignatureScriptSignature,
      MultiSignatureScriptPubKey,
      Seq[ECPrivateKey])] =
    for {
      (privateKeys, requiredSigs) <- CryptoGenerators.privateKeySeqWithRequiredSigs
      hashType <- CryptoGenerators.hashType
      publicKeys = privateKeys.map(_.publicKey)
      multiSigScriptPubKey = MultiSignatureScriptPubKey(requiredSigs,
                                                        publicKeys)
      emptyDigitalSignatures = privateKeys.map(_ => EmptyDigitalSignature)
      scriptSig = MultiSignatureScriptSignature(emptyDigitalSignatures)
      (creditingTx, outputIndex) = TransactionGenerators
        .buildCreditingTransaction(multiSigScriptPubKey)
      (spendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(
        creditingTx,
        scriptSig,
        outputIndex)
      txSigComponentFuture = MultiSigSigner.sign(
        privateKeys,
        creditingTx.outputs(outputIndex.toInt),
        spendingTx,
        inputIndex,
        hashType,
        false)
      txSigComponent = Await.result(txSigComponentFuture, timeout)
      signedScriptSig = txSigComponent.scriptSignature
        .asInstanceOf[MultiSignatureScriptSignature]
    } yield (signedScriptSig, multiSigScriptPubKey, privateKeys)

  /**
    * Generates a signed `P2SHScriptSignature`
    * that spends from a `P2SHScriptPubKey` correctly
    *
    * @return the signed `P2SHScriptSignature`,
    *         the `P2SHScriptPubKey` it spends, and the
    *         sequence of `ECPrivateKey`
    *         used to sign the scriptSig
    */
  def signedP2SHScriptSignature: Gen[
    (P2SHScriptSignature, P2SHScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (scriptSig, redeemScript, privateKeys) <- chooseSignedScriptSig
      p2SHScriptPubKey = P2SHScriptPubKey(redeemScript)
      p2SHScriptSignature = P2SHScriptSignature(scriptSig, redeemScript)
    } yield (p2SHScriptSignature, p2SHScriptPubKey, privateKeys)

  /**
    * @return the signed `CLTVScriptSignature`, the
    *         `CLTVScriptPubKey` it spends, and the
    *         sequences of `ECPrivateKey`
    *         used to sign the scriptSig
    */
  def signedCLTVScriptSignature(
      cltvLockTime: ScriptNumber,
      lockTime: UInt32,
      sequence: UInt32): Gen[
    (CLTVScriptSignature, CLTVScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (scriptPubKey, privKeys) <- randomNonLockTimeNonP2SHScriptPubKey
      hashType <- CryptoGenerators.hashType
      cltv = CLTVScriptPubKey(cltvLockTime, scriptPubKey)
    } yield
      scriptPubKey match {
        case m: MultiSignatureScriptPubKey =>
          val requiredSigs = m.requiredSigs
          val cltvScriptSig = lockTimeHelper(Some(lockTime),
                                             sequence,
                                             cltv,
                                             privKeys,
                                             Some(requiredSigs),
                                             hashType)
          (cltvScriptSig.asInstanceOf[CLTVScriptSignature], cltv, privKeys)
        case _: P2PKHScriptPubKey | _: P2PKScriptPubKey =>
          val cltvScriptSig = lockTimeHelper(Some(lockTime),
                                             sequence,
                                             cltv,
                                             privKeys,
                                             None,
                                             hashType)
          (cltvScriptSig.asInstanceOf[CLTVScriptSignature], cltv, privKeys)
        case _: UnassignedWitnessScriptPubKey | _: WitnessScriptPubKeyV0 =>
          throw new IllegalArgumentException(
            "Cannot created a witness scriptPubKey for a CSVScriptSig since we do not have a witness")
        case _: P2SHScriptPubKey | _: CLTVScriptPubKey | _: CSVScriptPubKey |
            _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey =>
          throw new IllegalArgumentException(
            "We only " +
              "want to generate P2PK, P2PKH, and MultiSig ScriptSignatures when creating a CSVScriptSignature")
      }

  /**
    * Generates a signed `CLTVScriptSignature` that spends
    * from a `CLTVScriptSignature` correctly
    *
    * @return the signed `CSVScriptSignature`, the
    *         `CSVScriptPubKey` it spends, and the
    *         sequences of `ECPrivateKey`
    *         used to sign the scriptSig
    */
  def signedCSVScriptSignature(
      csvScriptNum: ScriptNumber,
      sequence: UInt32): Gen[
    (CSVScriptSignature, CSVScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (scriptPubKey, privKeys) <- randomNonLockTimeNonP2SHScriptPubKey
      hashType <- CryptoGenerators.hashType
      csv = CSVScriptPubKey(csvScriptNum, scriptPubKey)
    } yield
      scriptPubKey match {
        case m: MultiSignatureScriptPubKey =>
          val requiredSigs = m.requiredSigs
          val csvScriptSig = lockTimeHelper(None,
                                            sequence,
                                            csv,
                                            privKeys,
                                            Some(requiredSigs),
                                            hashType)
          (csvScriptSig.asInstanceOf[CSVScriptSignature], csv, privKeys)
        case _: P2PKHScriptPubKey | _: P2PKScriptPubKey =>
          val csvScriptSig =
            lockTimeHelper(None, sequence, csv, privKeys, None, hashType)
          (csvScriptSig.asInstanceOf[CSVScriptSignature], csv, privKeys)
        case _: UnassignedWitnessScriptPubKey | _: WitnessScriptPubKeyV0 =>
          throw new IllegalArgumentException(
            "Cannot created a witness scriptPubKey for a CSVScriptSig since we do not have a witness")
        case _: P2SHScriptPubKey | _: CLTVScriptPubKey | _: CSVScriptPubKey |
            _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey =>
          throw new IllegalArgumentException(
            "We only " +
              "want to generate P2PK, P2PKH, and MultiSig ScriptSignatures when creating a CLTVScriptSignature.")
      }

  def signedCSVScriptSignature: Gen[
    (CSVScriptSignature, CSVScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (csv, privKeys) <- csvScriptPubKey
      sequence <- NumberGenerator.uInt32s
      scriptSig <- signedCSVScriptSignature(csv.locktime, sequence)
    } yield scriptSig

  def signedCLTVScriptSignature: Gen[
    (CLTVScriptSignature, CLTVScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (cltv, privKeys) <- cltvScriptPubKey
      txLockTime <- NumberGenerator.uInt32s
      sequence <- NumberGenerator.uInt32s
      scriptSig <- signedCLTVScriptSignature(cltv.locktime,
                                             txLockTime,
                                             sequence)
    } yield scriptSig

  /** Generates a `LockTimeScriptSignature` and
    * `LockTimeScriptPubKey` pair that are valid when
    * run through the interpreter */
  def signedLockTimeScriptSignature: Gen[
    (LockTimeScriptSignature, LockTimeScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedCSVScriptSignature, signedCLTVScriptSignature)
  }

  /** Helper function to generate LockTimeScriptSignatures */
  private def lockTimeHelper(
      lockTime: Option[UInt32],
      sequence: UInt32,
      lock: LockTimeScriptPubKey,
      privateKeys: Seq[ECPrivateKey],
      requiredSigs: Option[Int],
      hashType: HashType): LockTimeScriptSignature = {
    val tc = TransactionConstants
    val pubKeys = privateKeys.map(_.publicKey)
    val (creditingTx, outputIndex) =
      TransactionGenerators.buildCreditingTransaction(tc.validLockVersion, lock)
    val (unsignedSpendingTx, inputIndex) =
      TransactionGenerators.buildSpendingTransaction(
        tc.validLockVersion,
        creditingTx,
        EmptyScriptSignature,
        outputIndex,
        lockTime.getOrElse(tc.lockTime),
        sequence)
    val output = TransactionOutput(CurrencyUnits.zero, lock)
    val txSignatureComponent = BaseTxSigComponent(
      unsignedSpendingTx,
      inputIndex,
      output,
      Policy.standardScriptVerifyFlags)

    val txSignatures: Seq[ECDigitalSignature] = for {
      i <- 0 until requiredSigs.getOrElse(1)
    } yield
      TransactionSignatureCreator.createSig(txSignatureComponent,
                                            privateKeys(i),
                                            hashType)

    lock match {
      case csv: CSVScriptPubKey =>
        val nestedScriptSig =
          lockTimeHelperScriptSig(csv, txSignatures, pubKeys)
        CSVScriptSignature(nestedScriptSig)
      case cltv: CLTVScriptPubKey =>
        val nestedScriptSig =
          lockTimeHelperScriptSig(cltv, txSignatures, pubKeys)
        CLTVScriptSignature(nestedScriptSig)
    }
  }

  def signedP2SHP2WPKHScriptSignature: Gen[
    (
        P2SHScriptSignature,
        P2SHScriptPubKey,
        Seq[ECPrivateKey],
        TransactionWitness,
        CurrencyUnit)] =
    for {
      (witness, wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WPKHTransactionWitness
      p2shScriptPubKey = P2SHScriptPubKey(wtxSigComponent.scriptPubKey)
      p2shScriptSig = P2SHScriptSignature(
        wtxSigComponent.scriptPubKey.asInstanceOf[WitnessScriptPubKey])
    } yield
      (p2shScriptSig,
       p2shScriptPubKey,
       privKeys,
       witness,
       wtxSigComponent.amount)

  def signedP2SHP2WSHScriptSignature: Gen[
    (
        P2SHScriptSignature,
        P2SHScriptPubKey,
        Seq[ECPrivateKey],
        TransactionWitness,
        CurrencyUnit)] =
    for {
      (witness, wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WSHTransactionWitness
      p2shScriptPubKey = P2SHScriptPubKey(wtxSigComponent.scriptPubKey)
      p2shScriptSig = P2SHScriptSignature(wtxSigComponent.scriptPubKey)
    } yield
      (p2shScriptSig,
       p2shScriptPubKey,
       privKeys,
       witness,
       wtxSigComponent.amount)

  /**
    * This function chooses a random signed `ScriptSignature`
    * that is NOT a `P2SHScriptSignature`,
    * `CSVScriptSignature`,
    * `CLTVScriptSignature`, or any witness type
    *
    * @return the signed `ScriptSignature`,
    *         the `ScriptPubKey` it is spending,
    *         and the sequence of `ECPrivateKey` used to sign it
    */
  def chooseSignedScriptSig: Gen[
    (ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(
      packageToSequenceOfPrivateKeys(signedP2PKScriptSignature),
      packageToSequenceOfPrivateKeys(signedP2PKHScriptSignature),
      signedMultiSignatureScriptSignature
    )
  }

  /** Generates a random `ScriptSignature`, the
    * `ScriptPubKey` it is spending, and the
    * `ECPrivateKey` needed to spend it. */
  def randomScriptSig: Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])] = {
    val witP2SHP2WPKH =
      signedP2SHP2WPKHScriptSignature.map(x => (x._1, x._2, x._3))
    val witP2SHP2WSH =
      signedP2SHP2WSHScriptSignature.map(x => (x._1, x._2, x._3))
    Gen.oneOf(
      packageToSequenceOfPrivateKeys(signedP2PKHScriptSignature),
      packageToSequenceOfPrivateKeys(signedP2PKScriptSignature),
      signedMultiSignatureScriptSignature,
      signedCLTVScriptSignature,
      signedCSVScriptSignature,
      signedP2SHScriptSignature,
      witP2SHP2WPKH,
      witP2SHP2WSH
    )
  }

  /** Simply converts one private key in the generator to a sequence of private keys */
  private def packageToSequenceOfPrivateKeys(
      gen: Gen[(ScriptSignature, ScriptPubKey, ECPrivateKey)]): Gen[
    (ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])] =
    for {
      (scriptSig, scriptPubKey, privateKey) <- gen
    } yield (scriptSig, scriptPubKey, Seq(privateKey))

  /** Simply converts one private key in the generator to a sequence of private keys */
  private def privKeyToSeq(tuple: (ScriptPubKey, ECPrivateKey)): (
      ScriptPubKey,
      Seq[ECPrivateKey]) = {
    val (s, key) = tuple
    (s, Seq(key))
  }

  private def lockTimeHelperScriptSig(
      lock: LockTimeScriptPubKey,
      sigs: Seq[ECDigitalSignature],
      keys: Seq[ECPublicKey]): LockTimeScriptSignature = {

    val nestedScriptSig = lock.nestedScriptPubKey match {
      case _: P2PKScriptPubKey                                         => P2PKScriptSignature(sigs.head)
      case _: P2PKHScriptPubKey                                        => P2PKHScriptSignature(sigs.head, keys.head)
      case _: MultiSignatureScriptPubKey                               => MultiSignatureScriptSignature(sigs)
      case EmptyScriptPubKey                                           => CSVScriptSignature(EmptyScriptSignature)
      case _: WitnessScriptPubKeyV0 | _: UnassignedWitnessScriptPubKey =>
        //bare segwit always has an empty script sig, see BIP141
        CSVScriptSignature(EmptyScriptSignature)
      case _: LockTimeScriptPubKey =>
        throw new IllegalArgumentException(
          "Cannot have a nested locktimeScriptPubKey inside a lockTimeScriptPubKey")
      case x @ (_: NonStandardScriptPubKey | _: P2SHScriptPubKey |
          _: WitnessCommitment) =>
        throw new IllegalArgumentException(
          "A NonStandardScriptPubKey/P2SHScriptPubKey/WitnessCommitment cannot be" +
            "the underlying scriptSig in a CSVScriptSignature. Got: " + x)
    }

    lock match {
      case _: CLTVScriptPubKey => CLTVScriptSignature(nestedScriptSig)
      case _: CSVScriptPubKey  => CSVScriptSignature(nestedScriptSig)
    }
  }
}

object ScriptGenerators extends ScriptGenerators
