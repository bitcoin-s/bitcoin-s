package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{TransactionSignatureCreator, _}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{TransactionConstants, TransactionWitness}
import org.bitcoins.core.script.ScriptSettings
import org.bitcoins.core.script.constant.{OP_16, ScriptNumber}
import org.bitcoins.core.script.crypto.{HashType, SIGHASH_ALL}
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
    hexString <- StringGenerators.hexString
    signature = privKey.sign(hexString)
  } yield P2PKHScriptSignature(signature,privKey.publicKey)

  def multiSignatureScriptSignature : Gen[MultiSignatureScriptSignature] = {
    val signatures : Gen[Seq[ECDigitalSignature]] = for {
      numKeys <- Gen.choose(1, ScriptSettings.maxPublicKeysPerMultiSig)
      hexString <- StringGenerators.hexString
    } yield for {
      _ <- 0 until numKeys
      privKey = ECPrivateKey()
    } yield privKey.sign(hexString)
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
    randomHex <- StringGenerators.hexString
    sigs = privKeys.map(key => key.sign(randomHex))
  } yield {
    CLTVScriptSignature(cltv, sigs, pubKeys)
  }

  def csvScriptSignature : Gen[CSVScriptSignature] = for {
    num <- NumberGenerator.scriptNumbers
    (csv, privKeys, num) <- csvScriptPubKey(num)
    pubKeys = privKeys.map(_.publicKey)
    randomHex <- StringGenerators.hexString
    sigs = privKeys.map(key => key.sign(randomHex))
  } yield {
    CSVScriptSignature(csv, sigs, pubKeys)
  }

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
    amount <- CurrencyUnitGenerator.satoshis
  } yield (WitnessScriptPubKeyV0(privKey.publicKey), Seq(privKey))

  /** Creates an [[UnassignedWitnessScriptPubKey]],
    * currently this is any witness script pubkey besides [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0]
    */
  def unassignedWitnessScriptPubKey: Gen[(UnassignedWitnessScriptPubKey, Seq[ECPrivateKey])] = for {
    (witV0,privKeys) <- witnessScriptPubKeyV0
    unassignedAsm = OP_16 +: witV0.asm.tail
  } yield (UnassignedWitnessScriptPubKey(unassignedAsm),privKeys)

  def pickRandomNonP2SHScriptPubKey: Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(p2pkScriptPubKey.map(privKeytoSeq(_)), p2pkhScriptPubKey.map(privKeytoSeq(_)),
      cltvScriptPubKey.suchThat(!_._1.scriptPubKeyAfterCLTV.isInstanceOf[CSVScriptPubKey]),
      csvScriptPubKey.suchThat(!_._1.scriptPubKeyAfterCSV.isInstanceOf[CLTVScriptPubKey]),
      multiSigScriptPubKey
    )
  }

  def pickRandomNonCLTVNonCSVNonP2SHScriptPubKey : Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(p2pkScriptPubKey.map(privKeytoSeq(_)), p2pkhScriptPubKey.map(privKeytoSeq(_)),multiSigScriptPubKey)
  }

  /** Generates an arbitrary [[ScriptPubKey]] */
  def scriptPubKey : Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    Gen.oneOf(p2pkScriptPubKey.map(privKeytoSeq(_)),p2pkhScriptPubKey.map(privKeytoSeq(_)),
      multiSigScriptPubKey,emptyScriptPubKey,
      cltvScriptPubKey,csvScriptPubKey,witnessScriptPubKeyV0,unassignedWitnessScriptPubKey,
      p2shScriptPubKey)
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
    case cltv : CLTVScriptPubKey => cltvScriptSignature
    case csv : CSVScriptPubKey => csvScriptSignature
    case _ @ (_ : WitnessScriptPubKeyV0 | _ : UnassignedWitnessScriptPubKey) => emptyScriptSignature
    case x @ (_: P2SHScriptPubKey | _: NonStandardScriptPubKey) =>
      throw new IllegalArgumentException("Cannot pick for p2sh script pubkey, " +
        "non standard script pubkey, got: " + x)
  }

  /**
    * Generates a signed [[P2PKScriptSignature]] that spends the [[P2PKScriptPubKey]] correctly
    *
    * @return the signed [[P2PKScriptSignature]], the [[P2PKScriptPubKey]] it spends, and the
    *         [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedP2PKScriptSignature: Gen[(P2PKScriptSignature,P2PKScriptPubKey,ECPrivateKey, CurrencyUnit)] = for {
    privateKey <- CryptoGenerators.privateKey
  } yield {
    val publicKey = privateKey.publicKey
    val scriptPubKey = P2PKScriptPubKey(publicKey)
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    val scriptSig = P2PKScriptSignature(EmptyDigitalSignature)
    val (spendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx, scriptSig, outputIndex)
    val txSignatureComponent = TransactionSignatureComponent(spendingTx, inputIndex, scriptPubKey,
      Policy.standardScriptVerifyFlags)
    val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privateKey, HashType.sigHashAll)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = P2PKScriptSignature(txSignature)
    (signedScriptSig,scriptPubKey,privateKey,creditingTx.outputs(outputIndex.toInt).value)
  }

  /**
    * Generates a signed [[P2PKHScriptSignature]] that spends the [[P2PKHScriptPubKey]] correctly
    *
    * @return the signed [[P2PKHScriptSignature]], the [[P2PKHScriptPubKey]] it spends, and the
    *         [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedP2PKHScriptSignature: Gen[(P2PKHScriptSignature, P2PKHScriptPubKey, ECPrivateKey, CurrencyUnit)] = for {
    privateKey <- CryptoGenerators.privateKey
  } yield {
    val publicKey = privateKey.publicKey
    val scriptPubKey = P2PKHScriptPubKey(publicKey)
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    val scriptSig = P2PKHScriptSignature(EmptyDigitalSignature,publicKey)
    val (spendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx,scriptSig,outputIndex)
    val txSignatureComponent = TransactionSignatureComponent(spendingTx,inputIndex,scriptPubKey,
      Policy.standardScriptVerifyFlags)
    val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent,privateKey, HashType.sigHashAll)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = P2PKHScriptSignature(txSignature,publicKey)
    (signedScriptSig, scriptPubKey, privateKey,creditingTx.outputs(outputIndex.toInt).value)
  }

  /**
    * Generates a signed [[MultiSignatureScriptSignature]] that spends the [[MultiSignatureScriptPubKey]] correctly
    *ti
    * @return the signed [[MultiSignatureScriptSignature]], the [[MultiSignatureScriptPubKey]] it spends and the
    *         sequence of [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedMultiSignatureScriptSignature: Gen[(MultiSignatureScriptSignature, MultiSignatureScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = for {
    (privateKeys, requiredSigs) <- CryptoGenerators.privateKeySeqWithRequiredSigs
  } yield {
    val publicKeys = privateKeys.map(_.publicKey)
    val multiSigScriptPubKey = MultiSignatureScriptPubKey(requiredSigs,publicKeys)
    multiSigScriptSigGenHelper(privateKeys, requiredSigs, multiSigScriptPubKey)
  }

  /** Helps generate a signed [[MultiSignatureScriptSignature]] */
  private def multiSigScriptSigGenHelper(privateKeys : Seq[ECPrivateKey], requiredSigs : Int,
                                         scriptPubKey : MultiSignatureScriptPubKey) : (MultiSignatureScriptSignature, MultiSignatureScriptPubKey, Seq[ECPrivateKey], CurrencyUnit) = {
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    val emptyDigitalSignatures = privateKeys.map(_.publicKey).map(_ => EmptyDigitalSignature)
    val scriptSig = MultiSignatureScriptSignature(emptyDigitalSignatures)
    val (spendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx,scriptSig,outputIndex)
    val txSignatureComponent = TransactionSignatureComponent(spendingTx,inputIndex,
      scriptPubKey,Policy.standardScriptVerifyFlags)

    val txSignatures = for {
      i <- 0 until requiredSigs
    } yield TransactionSignatureCreator.createSig(txSignatureComponent,privateKeys(i), HashType.sigHashAll)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = MultiSignatureScriptSignature(txSignatures)
    (signedScriptSig, scriptPubKey, privateKeys,creditingTx.outputs(outputIndex.toInt).value)
  }

  /**
    * Generates a signed [[P2SHScriptSignature]] that spends from a [[P2SHScriptPubKey]] correctly
    *
    * @return the signed [[P2SHScriptSignature]], the [[P2SHScriptPubKey]] it spends, and the sequence of [[ECPrivateKey]]
    *         used to sign the scriptSig
    */
  def signedP2SHScriptSignature: Gen[(P2SHScriptSignature, P2SHScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = for {
    (scriptSig, redeemScript, privateKeys, amount) <- chooseSignedScriptSig
  } yield {
    val p2SHScriptPubKey = P2SHScriptPubKey(redeemScript)
    val p2SHScriptSignature = P2SHScriptSignature(scriptSig,redeemScript)
    (p2SHScriptSignature, p2SHScriptPubKey, privateKeys, amount)
  }

  /**
    * Generates a signed [[CLTVScriptSignature]] that spends from a [[CLTVScriptPubKey]] correctly
    *
    * @return the signed [[CLTVScriptSignature]], the [[CLTVScriptPubKey]] it spends, and the sequences of [[ECPrivateKey]]
    *         used to sign the scriptSig
    */
  def signedCLTVScriptSignature(cltvLockTime : ScriptNumber, lockTime : UInt32, sequence : UInt32) : Gen[(CLTVScriptSignature,
    CLTVScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = for {
    (scriptSig, scriptPubKey, privKeys, amount) <- chooseSignedScriptSig
    cltv = CLTVScriptPubKey(cltvLockTime, scriptPubKey)
  } yield scriptPubKey match {
      case _ : MultiSignatureScriptPubKey =>
        val requiredSigs = scriptSig.signatures.size
        val (cltvScriptSig, _, _) = cltvHelper(lockTime, sequence, cltv, privKeys, Some(requiredSigs))
        (cltvScriptSig, cltv, privKeys, amount)
      case _ : P2PKHScriptPubKey | _ : P2PKScriptPubKey =>
        val (cltvScriptSig, _, _) = cltvHelper(lockTime, sequence, cltv, privKeys, None)
        (cltvScriptSig, cltv, privKeys, amount)
      case x @ (_: UnassignedWitnessScriptPubKey | _: WitnessScriptPubKeyV0) =>
        (CLTVScriptSignature(x,Nil,privKeys.map(_.publicKey)), cltv, privKeys, amount)
      case _ : P2SHScriptPubKey | _ : CLTVScriptPubKey | _ : CSVScriptPubKey | _ : NonStandardScriptPubKey | EmptyScriptPubKey => throw new IllegalArgumentException("We only " +
        "want to generate P2PK, P2PKH, and MultiSig ScriptSignatures when creating a CSVScriptSignature. Got scriptSig: " + scriptSig)
  }

  /**
    * Generates a signed [[CSVScriptSignature]] that spends from a [[CSVScriptPubKey]] correctly
    *
    * @return the signed [[CSVScriptSignature]], the [[CSVScriptPubKey]] it spends, and the sequences of [[ECPrivateKey]]
    *         used to sign the scriptSig
    */
  def signedCSVScriptSignature(csvScriptNum : ScriptNumber, sequence : UInt32) : Gen[(CSVScriptSignature,
    CSVScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = for {
    (scriptSig, scriptPubKey, privKeys, amount) <- chooseSignedScriptSig
    csv = CSVScriptPubKey(csvScriptNum, scriptPubKey)
  } yield scriptPubKey match {
      case _ : MultiSignatureScriptPubKey =>
        val requiredSigs = scriptSig.signatures.size
        val (csvScriptSig, _, _) = csvHelper(sequence, csv, privKeys, Some(requiredSigs))
        (csvScriptSig, csv, privKeys, amount)
      case _ : P2PKHScriptPubKey | _ : P2PKScriptPubKey =>
        val (csvScriptSig, _, _) = csvHelper(sequence, csv, privKeys, None)
        (csvScriptSig, csv, privKeys, amount)
      case x @ (_: UnassignedWitnessScriptPubKey | _: WitnessScriptPubKeyV0) =>
        (CSVScriptSignature(x,Nil,privKeys.map(_.publicKey)), csv, privKeys, amount)
      case _ : P2SHScriptPubKey | _ : CLTVScriptPubKey | _ : CSVScriptPubKey | _ : NonStandardScriptPubKey | EmptyScriptPubKey => throw new IllegalArgumentException("We only " +
        "want to generate P2PK, P2PKH, and MultiSig ScriptSignatures when creating a CLTVScriptSignature.  Got scriptSig: " + scriptSig)
  }

  def signedCSVScriptSignature : Gen[(CSVScriptSignature, CSVScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = {
    val scriptSig = for {
      (csv, privKeys) <- csvScriptPubKey
      sequence <- NumberGenerator.uInt32s
    } yield signedCSVScriptSignature(csv.locktime, sequence)
    scriptSig.flatMap(sig => sig)
  }

  def signedCLTVScriptSignature : Gen[(CLTVScriptSignature, CLTVScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = {
    val scriptSig = for {
      (cltv, privKeys) <- cltvScriptPubKey
      txLockTime <- NumberGenerator.uInt32s
      sequence <- NumberGenerator.uInt32s
    } yield signedCLTVScriptSignature(cltv.locktime, txLockTime, sequence)
    scriptSig.flatMap(sig => sig)
  }

  /** Helper function to generate signed CLTVScriptSignatures with appropriate number of signatures. */
  private def cltvHelper(lockTime : UInt32, sequence : UInt32, cltv: CLTVScriptPubKey, privateKeys : Seq[ECPrivateKey], requiredSigs : Option[Int]) : (CLTVScriptSignature, CLTVScriptPubKey, Seq[ECPrivateKey]) = {
    val pubKeys = privateKeys.map(_.publicKey)
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(cltv)
    val (unsignedSpendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(TransactionConstants.version, creditingTx,
      EmptyScriptSignature, outputIndex, lockTime, sequence)

    val txSignatureComponent = TransactionSignatureComponent(unsignedSpendingTx, inputIndex,
      cltv, Policy.standardScriptVerifyFlags)
    val txSignatures : Seq[ECDigitalSignature] = for {
          i <- 0 until requiredSigs.getOrElse(1)
    } yield TransactionSignatureCreator.createSig(txSignatureComponent,privateKeys(i), HashType.sigHashAll)

    val signedScriptSig : CLTVScriptSignature = CLTVScriptSignature(cltv, txSignatures, pubKeys)
    (signedScriptSig, cltv, privateKeys)
  }

  /** Helper function to generate signed CSVScriptSignatures with appropriate number of signatures. */
  private def csvHelper(sequence : UInt32, csv: CSVScriptPubKey, privateKeys : Seq[ECPrivateKey], requiredSigs : Option[Int]) : (CSVScriptSignature, CSVScriptPubKey, Seq[ECPrivateKey]) = {
    val pubKeys = privateKeys.map(_.publicKey)
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(UInt32(2),csv)
    val (unsignedSpendingTx, inputIndex) = {
      TransactionGenerators.buildSpendingTransaction(UInt32(2), creditingTx, EmptyScriptSignature, outputIndex, UInt32.zero, sequence)
    }

    val txSignatureComponent = TransactionSignatureComponent(unsignedSpendingTx, inputIndex,
      csv, Policy.standardScriptVerifyFlags)
    val txSignatures : Seq[ECDigitalSignature] = for {
      i <- 0 until requiredSigs.getOrElse(1)
    } yield TransactionSignatureCreator.createSig(txSignatureComponent,privateKeys(i), HashType.sigHashAll)

    val signedScriptSig : CSVScriptSignature = CSVScriptSignature(csv, txSignatures, pubKeys)
    (signedScriptSig, csv, privateKeys)
  }

  def signedP2SHP2WPKHScriptSignature: Gen[(P2SHScriptSignature, P2SHScriptPubKey, Seq[ECPrivateKey], TransactionWitness, CurrencyUnit)] = for {
    (witness, wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WPKHTransactionWitness
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
  def chooseSignedScriptSig: Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = {
    Gen.oneOf(packageToSequenceOfPrivateKeys(signedP2PKScriptSignature),
      packageToSequenceOfPrivateKeys(signedP2PKHScriptSignature),
      signedMultiSignatureScriptSignature)
  }

  /** Generates a random [[ScriptSignature]], the [[ScriptPubKey]]/[[CurrencyUnit]] it is spending, and the [[ECPrivateKey]] needed to spend it. */
  def randomScriptSig : Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = {
    Gen.oneOf(packageToSequenceOfPrivateKeys(signedP2PKHScriptSignature),
      packageToSequenceOfPrivateKeys(signedP2PKScriptSignature),
      signedMultiSignatureScriptSignature, signedCLTVScriptSignature,
      signedCSVScriptSignature,signedP2SHScriptSignature)
  }

  /** Simply converts one private key in the generator to a sequence of private keys */
  private def packageToSequenceOfPrivateKeys(gen: Gen[(ScriptSignature, ScriptPubKey, ECPrivateKey, CurrencyUnit)]): Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey], CurrencyUnit)] = for {
    (scriptSig, scriptPubKey, privateKey, amount) <- gen
  } yield (scriptSig, scriptPubKey, Seq(privateKey), amount)

  /** Simply converts one private key in the generator to a sequence of private keys */
  private def privKeytoSeq(tuple :(ScriptPubKey, ECPrivateKey)): (ScriptPubKey, Seq[ECPrivateKey]) = {
    val (s,key) = tuple
    (s,Seq(key))
  }
}

object ScriptGenerators extends ScriptGenerators
