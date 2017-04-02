package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{TransactionSignatureCreator, _}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{P2SHScriptPubKey, _}
import org.bitcoins.core.protocol.transaction.{TransactionConstants, TransactionWitness}
import org.bitcoins.core.script.ScriptSettings
import org.bitcoins.core.script.constant.{OP_16, ScriptNumber}
import org.bitcoins.core.script.crypto.{HashType, SIGHASH_ALL}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.Gen

/**
  * Created by chris on 6/22/16.
  */
//TODO: Need to provide generators for [[NonStandardScriptSignature]] and [[NonStandardScriptPubKey]]
trait ScriptGenerators extends BitcoinSLogger {


  def p2pkScriptSignature : Gen[P2PKScriptSignature] = for {
    digitalSignature <- CryptoGenerators.digitalSignatures
  } yield P2PKScriptSignature(digitalSignature)

  def p2pkhScriptSignature : Gen[P2PKHScriptSignature] = for {
    privKey <- CryptoGenerators.privateKey
    hash <- CryptoGenerators.doubleSha256Digest
    signature = privKey.sign(hash)
  } yield P2PKHScriptSignature(signature,privKey.publicKey)

  def multiSignatureScriptSignature : Gen[MultiSignatureScriptSignature] = {
    val signatures : Gen[Seq[ECDigitalSignature]] = for {
      numKeys <- Gen.choose(1, ScriptSettings.maxPublicKeysPerMultiSig)
      hash <- CryptoGenerators.doubleSha256Digest
    } yield for {
      _ <- 0 until numKeys
      privKey = ECPrivateKey()
    } yield privKey.sign(hash)
    signatures.map(sigs => MultiSignatureScriptSignature(sigs))
  }

  def emptyScriptSignature = p2pkhScriptSignature.map(_ => EmptyScriptSignature)

  /**
    * Generates a [[org.bitcoins.core.protocol.script.P2SHScriptSignature]]
    * WARNING: the redeem script and the script signature DO NOT evaluate to true
    * if executed by [[org.bitcoins.core.script.interpreter.ScriptInterpreter]]
    */
  def p2shScriptSignature : Gen[P2SHScriptSignature] = for {
    (scriptPubKey, _) <- pickRandomNonP2SHScriptPubKey
    scriptSig <- pickCorrespondingScriptSignature(scriptPubKey)
    p2shScriptSig = P2SHScriptSignature(scriptSig, scriptPubKey)
  } yield p2shScriptSig

  def cltvScriptSignature : Gen[CLTVScriptSignature] = for {
    num <- NumberGenerator.scriptNumbers
    (cltv, privKeys, num) <- cltvScriptPubKey(num)
    pubKeys = privKeys.map(_.publicKey)
    hash <- CryptoGenerators.doubleSha256Digest
    sigs = privKeys.map(key => key.sign(hash))
  } yield CLTVScriptSignature(cltv, sigs, pubKeys)


  def csvScriptSignature : Gen[CSVScriptSignature] = for {
    num <- NumberGenerator.scriptNumbers
    (csv, privKeys, num) <- csvScriptPubKey(num)
    pubKeys = privKeys.map(_.publicKey)
    hash <- CryptoGenerators.doubleSha256Digest
    sigs = privKeys.map(key => key.sign(hash))
  } yield CSVScriptSignature(csv, sigs, pubKeys)


  def p2pkScriptPubKey : Gen[(P2PKScriptPubKey, ECPrivateKey)] = for {
    privKey <- CryptoGenerators.privateKey
    pubKey = privKey.publicKey
    p2pk = P2PKScriptPubKey(pubKey)
  } yield (p2pk,privKey)

  def p2pkhScriptPubKey : Gen[(P2PKHScriptPubKey, ECPrivateKey)] = for {
    privKey <- CryptoGenerators.privateKey
    pubKey = privKey.publicKey
    p2pkh = P2PKHScriptPubKey(pubKey)
  } yield (p2pkh,privKey)

  def cltvScriptPubKey : Gen[(CLTVScriptPubKey, Seq[ECPrivateKey])] = for {
    num <- NumberGenerator.scriptNumbers
    (cltv, privKeys, num) <- cltvScriptPubKey(num)
  } yield (cltv, privKeys)

  def cltvScriptPubKey(num : ScriptNumber) : Gen[(CLTVScriptPubKey, Seq[ECPrivateKey], ScriptNumber)] = for {
    (scriptPubKey, privKeys) <- pickRandomNonCLTVNonCSVNonP2SHScriptPubKey
  } yield {
    val cltv = CLTVScriptPubKey(num, scriptPubKey)
    (cltv, privKeys, num)
  }

  def csvScriptPubKey(num : ScriptNumber) : Gen[(CSVScriptPubKey, Seq[ECPrivateKey], ScriptNumber)] = for {
    (scriptPubKey, privKeys) <- pickRandomNonCLTVNonCSVNonP2SHScriptPubKey
  } yield {
    val csv = CSVScriptPubKey(num, scriptPubKey)
    (csv, privKeys, num)
  }

  def csvScriptPubKey : Gen[(CSVScriptPubKey, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- pickRandomNonCLTVNonCSVNonP2SHScriptPubKey
    num <- NumberGenerator.scriptNumbers
    csv = CSVScriptPubKey(num, scriptPubKey)
  } yield (csv, privKeys)

  def multiSigScriptPubKey : Gen[(MultiSignatureScriptPubKey, Seq[ECPrivateKey])] = for {
    (privateKeys, requiredSigs) <- CryptoGenerators.privateKeySeqWithRequiredSigs
    pubKeys = privateKeys.map(_.publicKey)
    multiSignatureScriptPubKey = MultiSignatureScriptPubKey(requiredSigs, pubKeys)
  } yield (multiSignatureScriptPubKey, privateKeys)

  def p2shScriptPubKey : Gen[(P2SHScriptPubKey, Seq[ECPrivateKey])] = for {
    (randomScriptPubKey, privKeys) <- pickRandomNonP2SHScriptPubKey
    p2sh = P2SHScriptPubKey(randomScriptPubKey)
  } yield (p2sh, privKeys)

  def emptyScriptPubKey : Gen [(ScriptPubKey, Seq[ECPrivateKey])] = (EmptyScriptPubKey, Nil)

  /** Creates a basic version 0 P2WPKH scriptpubkey */
  def witnessScriptPubKeyV0: Gen[(WitnessScriptPubKeyV0,Seq[ECPrivateKey])] = for {
    privKey <- CryptoGenerators.privateKey
  } yield (WitnessScriptPubKeyV0(privKey.publicKey), Seq(privKey))

  /** Creates an [[UnassignedWitnessScriptPubKey]],
    * currently this is any witness script pubkey besides [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0]
    */
  def unassignedWitnessScriptPubKey: Gen[(UnassignedWitnessScriptPubKey, Seq[ECPrivateKey])] = for {
    (witV0,privKeys) <- witnessScriptPubKeyV0
    unassignedAsm = OP_16 +: witV0.asm.tail
  } yield (UnassignedWitnessScriptPubKey(unassignedAsm),privKeys)

  /** Generates an arbitrary [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]] */
  def witnessScriptPubKey: Gen[(WitnessScriptPubKey, Seq[ECPrivateKey])] = Gen.oneOf(witnessScriptPubKeyV0,unassignedWitnessScriptPubKey)

  def witnessCommitment: Gen[(WitnessCommitment, Seq[ECPrivateKey])] = for {
    hash <- CryptoGenerators.doubleSha256Digest
  } yield (WitnessCommitment(hash),Nil)

  def escrowTimeoutScriptPubKey: Gen[(EscrowTimeoutScriptPubKey, Seq[ECPrivateKey])] = for {
    (escrow,k1) <- ScriptGenerators.multiSigScriptPubKey
    (timeout,k2) <- ScriptGenerators.csvScriptPubKey
  } yield (EscrowTimeoutScriptPubKey(escrow,timeout), k1 ++ k2)


  def escrowTimeoutScriptSig: Gen[EscrowTimeoutScriptSignature] = for {
    scriptSig <- Gen.oneOf(csvScriptSignature, multiSignatureScriptSignature)
    bool = if (scriptSig.isInstanceOf[MultiSignatureScriptSignature]) OP_1 else OP_0
  } yield EscrowTimeoutScriptSignature.fromAsm(bool +: scriptSig.asm)

  def pickRandomNonP2SHScriptPubKey: Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(p2pkScriptPubKey.map(privKeyToSeq(_)), p2pkhScriptPubKey.map(privKeyToSeq(_)),
      cltvScriptPubKey.suchThat(!_._1.nestedScriptPubKey.isInstanceOf[CSVScriptPubKey]),
      csvScriptPubKey.suchThat(!_._1.nestedScriptPubKey.isInstanceOf[CLTVScriptPubKey]),
      multiSigScriptPubKey, witnessScriptPubKeyV0, unassignedWitnessScriptPubKey, escrowTimeoutScriptPubKey
    )
  }

  /** This is used for creating time locked scriptPubKeys, we cannot nest CSV/CLTV/P2SH/Witness
    * ScriptPubKeys inside of timelock scriptPubKeys */
  def pickRandomNonCLTVNonCSVNonP2SHScriptPubKey : Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(p2pkScriptPubKey.map(privKeyToSeq(_)),
      p2pkhScriptPubKey.map(privKeyToSeq(_)),
      multiSigScriptPubKey)
  }

  /** Generates an arbitrary [[ScriptPubKey]] */
  def scriptPubKey : Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(p2pkScriptPubKey.map(privKeyToSeq(_)),p2pkhScriptPubKey.map(privKeyToSeq(_)),
      multiSigScriptPubKey,emptyScriptPubKey,
      cltvScriptPubKey,csvScriptPubKey,witnessScriptPubKeyV0,unassignedWitnessScriptPubKey,
      p2shScriptPubKey, witnessCommitment, escrowTimeoutScriptPubKey)
  }

  /** Generates an arbitrary [[ScriptSignature]] */
  def scriptSignature : Gen[ScriptSignature] = {
    Gen.oneOf(p2pkScriptSignature,p2pkhScriptSignature,multiSignatureScriptSignature,
      emptyScriptSignature,p2shScriptSignature)
  }

  /**
    * Generates a [[ScriptSignature]] corresponding to the type of [[ScriptPubKey]] given.
    * Note: Does NOT generate a correct/valid signature
    */
  private def pickCorrespondingScriptSignature(scriptPubKey : ScriptPubKey): Gen[ScriptSignature] = scriptPubKey match {
    case p2pk : P2PKScriptPubKey => p2pkScriptSignature
    case p2pkh : P2PKHScriptPubKey => p2pkhScriptSignature
    case multisig : MultiSignatureScriptPubKey => multiSignatureScriptSignature
    case EmptyScriptPubKey => emptyScriptSignature
    case _: CLTVScriptPubKey => cltvScriptSignature
    case _: CSVScriptPubKey => csvScriptSignature
    case _: EscrowTimeoutScriptPubKey => escrowTimeoutScriptSig
    case _: WitnessScriptPubKeyV0 | _ : UnassignedWitnessScriptPubKey => emptyScriptSignature
    case x @ (_: P2SHScriptPubKey | _: NonStandardScriptPubKey | _ : WitnessCommitment) =>
      throw new IllegalArgumentException("Cannot pick for p2sh script pubkey, " +
        "non standard script pubkey or witness commitment got: " + x)
  }

  /**
    * Generates a signed [[P2PKScriptSignature]] that spends the [[P2PKScriptPubKey]] correctly
    *
    * @return the signed [[P2PKScriptSignature]], the [[P2PKScriptPubKey]] it spends, and the
    *         [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedP2PKScriptSignature: Gen[(P2PKScriptSignature,P2PKScriptPubKey,ECPrivateKey)] = for {
    privateKey <- CryptoGenerators.privateKey
    hashType <- CryptoGenerators.hashType
    publicKey = privateKey.publicKey
    scriptPubKey = P2PKScriptPubKey(publicKey)
    (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    scriptSig = P2PKScriptSignature(EmptyDigitalSignature)
    (spendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx, scriptSig, outputIndex)
    txSignatureComponent = TxSigComponent(spendingTx, inputIndex, scriptPubKey,
      Policy.standardScriptVerifyFlags)
    txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privateKey, hashType)
    //add the signature to the scriptSig instead of having an empty scriptSig
    signedScriptSig = P2PKScriptSignature(txSignature)
  } yield (signedScriptSig,scriptPubKey,privateKey)


  /**
    * Generates a signed [[P2PKHScriptSignature]] that spends the [[P2PKHScriptPubKey]] correctly
    *
    * @return the signed [[P2PKHScriptSignature]], the [[P2PKHScriptPubKey]] it spends, and the
    *         [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedP2PKHScriptSignature: Gen[(P2PKHScriptSignature, P2PKHScriptPubKey, ECPrivateKey)] = for {
    privateKey <- CryptoGenerators.privateKey
    hashType <- CryptoGenerators.hashType
    publicKey = privateKey.publicKey
    scriptPubKey = P2PKHScriptPubKey(publicKey)
    (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    scriptSig = P2PKHScriptSignature(EmptyDigitalSignature,publicKey)
    (spendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx,scriptSig,outputIndex)
    txSignatureComponent = TxSigComponent(spendingTx,inputIndex,scriptPubKey,
      Policy.standardScriptVerifyFlags)
    txSignature = TransactionSignatureCreator.createSig(txSignatureComponent,privateKey, hashType)
    //add the signature to the scriptSig instead of having an empty scriptSig
    signedScriptSig = P2PKHScriptSignature(txSignature,publicKey)
  } yield (signedScriptSig, scriptPubKey, privateKey)

  /**
    * Generates a signed [[MultiSignatureScriptSignature]] that spends the [[MultiSignatureScriptPubKey]] correctly
    *ti
    * @return the signed [[MultiSignatureScriptSignature]], the [[MultiSignatureScriptPubKey]] it spends and the
    *         sequence of [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedMultiSignatureScriptSignature: Gen[(MultiSignatureScriptSignature, MultiSignatureScriptPubKey, Seq[ECPrivateKey])] = for {
    (privateKeys, requiredSigs) <- CryptoGenerators.privateKeySeqWithRequiredSigs
    hashType <- CryptoGenerators.hashType
    publicKeys = privateKeys.map(_.publicKey)
    multiSigScriptPubKey = MultiSignatureScriptPubKey(requiredSigs,publicKeys)
  } yield multiSigScriptSigGenHelper(privateKeys, multiSigScriptPubKey, hashType)

  /** Helps generate a signed [[MultiSignatureScriptSignature]] */
  private def multiSigScriptSigGenHelper(privateKeys : Seq[ECPrivateKey],
                                         scriptPubKey : MultiSignatureScriptPubKey, hashType: HashType) : (MultiSignatureScriptSignature, MultiSignatureScriptPubKey, Seq[ECPrivateKey]) = {
    val requiredSigs = scriptPubKey.requiredSigs
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    val emptyDigitalSignatures = privateKeys.map(_ => EmptyDigitalSignature)
    val scriptSig = MultiSignatureScriptSignature(emptyDigitalSignatures)
    val (spendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx,scriptSig,outputIndex)
    val txSignatureComponent = TxSigComponent(spendingTx,inputIndex,
      scriptPubKey,Policy.standardScriptVerifyFlags)

    val txSignatures = for {
      i <- 0 until requiredSigs
    } yield TransactionSignatureCreator.createSig(txSignatureComponent, privateKeys(i), hashType)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = MultiSignatureScriptSignature(txSignatures)
    (signedScriptSig, scriptPubKey, privateKeys)
  }

  /**
    * Generates a signed [[P2SHScriptSignature]] that spends from a [[P2SHScriptPubKey]] correctly
    *
    * @return the signed [[P2SHScriptSignature]], the [[P2SHScriptPubKey]] it spends, and the sequence of [[ECPrivateKey]]
    *         used to sign the scriptSig
    */
  def signedP2SHScriptSignature: Gen[(P2SHScriptSignature, P2SHScriptPubKey, Seq[ECPrivateKey])] = for {
    (scriptSig, redeemScript, privateKeys) <- chooseSignedScriptSig
    p2SHScriptPubKey = P2SHScriptPubKey(redeemScript)
    p2SHScriptSignature = P2SHScriptSignature(scriptSig,redeemScript)
  } yield (p2SHScriptSignature, p2SHScriptPubKey, privateKeys)

  /**
    * Generates a signed [[CLTVScriptSignature]] that spends from a [[CLTVScriptPubKey]] correctly
    *
    * @return the signed [[CLTVScriptSignature]], the [[CLTVScriptPubKey]] it spends, and the sequences of [[ECPrivateKey]]
    *         used to sign the scriptSig
    */
  def signedCLTVScriptSignature(cltvLockTime : ScriptNumber, lockTime : UInt32, sequence : UInt32) : Gen[(CLTVScriptSignature,
    CLTVScriptPubKey, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- pickRandomNonCLTVNonCSVNonP2SHScriptPubKey
    hashType <- CryptoGenerators.hashType
    cltv = CLTVScriptPubKey(cltvLockTime, scriptPubKey)
  } yield scriptPubKey match {
      case m : MultiSignatureScriptPubKey =>
        val requiredSigs = m.requiredSigs
        val (cltvScriptSig, _, _) = cltvHelper(lockTime, sequence, cltv, privKeys, Some(requiredSigs), hashType)
        (cltvScriptSig, cltv, privKeys)
      case _ : P2PKHScriptPubKey | _ : P2PKScriptPubKey =>
        val (cltvScriptSig, _, _) = cltvHelper(lockTime, sequence, cltv, privKeys, None, hashType)
        (cltvScriptSig, cltv, privKeys)
      case _: UnassignedWitnessScriptPubKey | _: WitnessScriptPubKeyV0 =>
        throw new IllegalArgumentException("Cannot created a witness scriptPubKey for a CSVScriptSig since we do not have a witness")
      case _ : P2SHScriptPubKey | _ : CLTVScriptPubKey | _ : CSVScriptPubKey | _ : NonStandardScriptPubKey
           | _ : WitnessCommitment | EmptyScriptPubKey => throw new IllegalArgumentException("We only " +
        "want to generate P2PK, P2PKH, and MultiSig ScriptSignatures when creating a CSVScriptSignature")
  }

  /**
    * Generates a signed [[CSVScriptSignature]] that spends from a [[CSVScriptPubKey]] correctly
    *
    * @return the signed [[CSVScriptSignature]], the [[CSVScriptPubKey]] it spends, and the sequences of [[ECPrivateKey]]
    *         used to sign the scriptSig
    */
  def signedCSVScriptSignature(csvScriptNum : ScriptNumber, sequence : UInt32) : Gen[(CSVScriptSignature,
    CSVScriptPubKey, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- pickRandomNonCLTVNonCSVNonP2SHScriptPubKey
    hashType <- CryptoGenerators.hashType
    csv = CSVScriptPubKey(csvScriptNum, scriptPubKey)
  } yield scriptPubKey match {
      case m : MultiSignatureScriptPubKey =>
        val requiredSigs = m.requiredSigs
        val (csvScriptSig, _, _) = csvHelper(sequence, csv, privKeys, Some(requiredSigs), hashType)
        (csvScriptSig, csv, privKeys)
      case _ : P2PKHScriptPubKey | _ : P2PKScriptPubKey =>
        val (csvScriptSig, _, _) = csvHelper(sequence, csv, privKeys, None, hashType)
        (csvScriptSig, csv, privKeys)
      case _: UnassignedWitnessScriptPubKey | _: WitnessScriptPubKeyV0 =>
        throw new IllegalArgumentException("Cannot created a witness scriptPubKey for a CSVScriptSig since we do not have a witness")
      case _ : P2SHScriptPubKey | _ : CLTVScriptPubKey | _ : CSVScriptPubKey | _ : NonStandardScriptPubKey
           | _ : WitnessCommitment | EmptyScriptPubKey => throw new IllegalArgumentException("We only " +
        "want to generate P2PK, P2PKH, and MultiSig ScriptSignatures when creating a CLTVScriptSignature.")
  }

  def signedCSVScriptSignature : Gen[(CSVScriptSignature, CSVScriptPubKey, Seq[ECPrivateKey])] = for {
    (csv, privKeys) <- csvScriptPubKey
    sequence <- NumberGenerator.uInt32s
    scriptSig <- signedCSVScriptSignature(csv.locktime, sequence)
  } yield scriptSig


  def signedCLTVScriptSignature : Gen[(CLTVScriptSignature, CLTVScriptPubKey, Seq[ECPrivateKey])] = for {
    (cltv, privKeys) <- cltvScriptPubKey
    txLockTime <- NumberGenerator.uInt32s
    sequence <- NumberGenerator.uInt32s
    scriptSig <- signedCLTVScriptSignature(cltv.locktime, txLockTime, sequence)
  } yield scriptSig

  def signedMultiSigEscrowTimeoutScriptSig(sequence: UInt32): Gen[(EscrowTimeoutScriptSignature, EscrowTimeoutScriptPubKey, Seq[ECPrivateKey])] = for {
    (_,csvScriptPubkey,_) <- signedCSVScriptSignature
    (_, multiSigScriptPubKey,multiSigPrivKeys) <- signedMultiSignatureScriptSignature
    hashType <- CryptoGenerators.hashType
    csvEscrowTimeout = EscrowTimeoutScriptPubKey(multiSigScriptPubKey,csvScriptPubkey)
    scriptSig = csvEscrowTimeoutHelper(sequence,csvEscrowTimeout,multiSigPrivKeys,
      Some(multiSigScriptPubKey.requiredSigs),hashType,true)
  } yield (scriptSig,csvEscrowTimeout,multiSigPrivKeys)

  def spendableTimeoutEscrowTimeoutScriptSig(scriptNum: ScriptNumber, sequence: UInt32): Gen[(EscrowTimeoutScriptSignature, EscrowTimeoutScriptPubKey, Seq[ECPrivateKey])] = for {
    (_,csv,csvPrivKeys) <- signedCSVScriptSignature(scriptNum, sequence)
    (_, multiSigScriptPubKey,_) <- signedMultiSignatureScriptSignature
    hashType <- CryptoGenerators.hashType
    csvEscrowTimeout = EscrowTimeoutScriptPubKey(multiSigScriptPubKey,csv)
    requireSigs = if (csv.nestedScriptPubKey.isInstanceOf[MultiSignatureScriptPubKey]) {
      val m = csv.nestedScriptPubKey.asInstanceOf[MultiSignatureScriptPubKey]
      Some(m.requiredSigs)
    } else None
    scriptSig = csvEscrowTimeoutHelper(sequence,csvEscrowTimeout,csvPrivKeys,requireSigs,hashType,false)
  } yield (scriptSig,csvEscrowTimeout,csvPrivKeys)

  /** Helper function to generate signed CLTVScriptSignatures with appropriate number of signatures. */
  private def cltvHelper(lockTime : UInt32, sequence : UInt32, cltv: CLTVScriptPubKey, privateKeys : Seq[ECPrivateKey],
                         requiredSigs : Option[Int], hashType: HashType) : (CLTVScriptSignature, CLTVScriptPubKey, Seq[ECPrivateKey]) = {
    val pubKeys = privateKeys.map(_.publicKey)
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(cltv)
    val (unsignedSpendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(TransactionConstants.version, creditingTx,
      EmptyScriptSignature, outputIndex, lockTime, sequence)

    val txSignatureComponent = TxSigComponent(unsignedSpendingTx, inputIndex,
      cltv, Policy.standardScriptVerifyFlags)
    val txSignatures : Seq[ECDigitalSignature] = for {
      i <- 0 until requiredSigs.getOrElse(1)
    } yield TransactionSignatureCreator.createSig(txSignatureComponent,privateKeys(i), hashType)

    val signedScriptSig : CLTVScriptSignature = CLTVScriptSignature(cltv, txSignatures, pubKeys)
    (signedScriptSig, cltv, privateKeys)
  }

  /** Helper function to generate signed CSVScriptSignatures with appropriate number of signatures. */
  private def csvHelper(sequence : UInt32, csv: CSVScriptPubKey, privateKeys : Seq[ECPrivateKey], requiredSigs : Option[Int], hashType: HashType) : (CSVScriptSignature, CSVScriptPubKey, Seq[ECPrivateKey]) = {
    val pubKeys = privateKeys.map(_.publicKey)
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(UInt32(2),csv)
    val (unsignedSpendingTx, inputIndex) = {
      TransactionGenerators.buildSpendingTransaction(UInt32(2), creditingTx, EmptyScriptSignature, outputIndex, UInt32.zero, sequence)
    }

    val txSignatureComponent = TxSigComponent(unsignedSpendingTx, inputIndex,
      csv, Policy.standardScriptVerifyFlags)
    val txSignatures : Seq[ECDigitalSignature] = for {
      i <- 0 until requiredSigs.getOrElse(1)
    } yield TransactionSignatureCreator.createSig(txSignatureComponent,privateKeys(i), hashType)

    val signedScriptSig : CSVScriptSignature = CSVScriptSignature(csv, txSignatures, pubKeys)
    (signedScriptSig, csv, privateKeys)
  }

  private def csvEscrowTimeoutHelper(sequence: UInt32, csvEscrowTimeout: EscrowTimeoutScriptPubKey, privateKeys: Seq[ECPrivateKey],
                                     requiredSigs: Option[Int], hashType: HashType, isMultiSig: Boolean) : EscrowTimeoutScriptSignature = {
    val pubKeys = privateKeys.map(_.publicKey)
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(UInt32(2),csvEscrowTimeout)
    val (unsignedSpendingTx, inputIndex) = {
      TransactionGenerators.buildSpendingTransaction(UInt32(2), creditingTx, EmptyScriptSignature, outputIndex, UInt32.zero, sequence)
    }
    val txSignatureComponent = TransactionSignatureComponent(unsignedSpendingTx, inputIndex,
      csvEscrowTimeout, Policy.standardScriptVerifyFlags)
    val txSignatures : Seq[ECDigitalSignature] = for {
      i <- 0 until requiredSigs.getOrElse(1)
    } yield TransactionSignatureCreator.createSig(txSignatureComponent,privateKeys(i), hashType)
    if (isMultiSig) {
      EscrowTimeoutScriptSignature(MultiSignatureScriptSignature(txSignatures))
    } else {
      EscrowTimeoutScriptSignature(CSVScriptSignature(csvEscrowTimeout,txSignatures,pubKeys))
    }
  }
  def signedP2SHP2WPKHScriptSignature: Gen[(P2SHScriptSignature, P2SHScriptPubKey, Seq[ECPrivateKey], TransactionWitness, CurrencyUnit)] = for {
    (witness, wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WPKHTransactionWitness
    p2shScriptPubKey = P2SHScriptPubKey(wtxSigComponent.scriptPubKey)
    p2shScriptSig = P2SHScriptSignature(wtxSigComponent.scriptPubKey.asInstanceOf[WitnessScriptPubKey])
  } yield (p2shScriptSig, p2shScriptPubKey, privKeys, witness, wtxSigComponent.amount)

  def signedP2SHP2WSHScriptSignature: Gen[(P2SHScriptSignature, P2SHScriptPubKey, Seq[ECPrivateKey], TransactionWitness, CurrencyUnit)] = for {
    (witness,wtxSigComponent,privKeys) <- WitnessGenerators.signedP2WSHTransactionWitness
    p2shScriptPubKey = P2SHScriptPubKey(wtxSigComponent.scriptPubKey)
    p2shScriptSig = P2SHScriptSignature(wtxSigComponent.scriptPubKey.asInstanceOf[WitnessScriptPubKey])
  } yield (p2shScriptSig, p2shScriptPubKey, privKeys, witness, wtxSigComponent.amount)

  /**
    * This function chooses a random signed [[ScriptSignature]] that is NOT a [[P2SHScriptSignature]], [[CSVScriptSignature]],
    * [[CLTVScriptSignature]], or any witness type
    *
    * @return the signed [[ScriptSignature]], the [[ScriptPubKey]] it is spending,
    *         and the sequence of[[ECPrivateKey]] used to sign it
    */
  def chooseSignedScriptSig: Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(packageToSequenceOfPrivateKeys(signedP2PKScriptSignature),
      packageToSequenceOfPrivateKeys(signedP2PKHScriptSignature),
      signedMultiSignatureScriptSignature)
  }

  /** Generates a random [[ScriptSignature]], the [[ScriptPubKey]] it is spending, and the [[ECPrivateKey]] needed to spend it. */
  def randomScriptSig : Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])] = {
    val witP2SHP2WPKH = signedP2SHP2WPKHScriptSignature.map(x => (x._1,x._2,x._3))
    val witP2SHP2WSH = signedP2SHP2WSHScriptSignature.map(x => (x._1,x._2,x._3))
    Gen.oneOf(packageToSequenceOfPrivateKeys(signedP2PKHScriptSignature),
      packageToSequenceOfPrivateKeys(signedP2PKScriptSignature),
      signedMultiSignatureScriptSignature, signedCLTVScriptSignature,
      signedCSVScriptSignature,signedP2SHScriptSignature,witP2SHP2WPKH,witP2SHP2WSH)
  }

  /** Simply converts one private key in the generator to a sequence of private keys */
  private def packageToSequenceOfPrivateKeys(gen: Gen[(ScriptSignature, ScriptPubKey, ECPrivateKey)]): Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])] = for {
    (scriptSig, scriptPubKey, privateKey) <- gen
  } yield (scriptSig, scriptPubKey, Seq(privateKey))

  /** Simply converts one private key in the generator to a sequence of private keys */
  private def privKeyToSeq(tuple :(ScriptPubKey, ECPrivateKey)): (ScriptPubKey, Seq[ECPrivateKey]) = {
    val (s,key) = tuple
    (s,Seq(key))
  }
}

object ScriptGenerators extends ScriptGenerators
