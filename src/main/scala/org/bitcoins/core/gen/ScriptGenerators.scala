package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{TransactionSignatureCreator, _}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.script.ScriptSettings
import org.bitcoins.core.script.constant.{ScriptConstant, ScriptNumber}
import org.bitcoins.core.script.crypto.SIGHASH_ALL
import org.bitcoins.core.util.{BitcoinScriptUtil, BitcoinSLogger}
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
    *
    * @return
    */
  def p2shScriptSignature : Gen[P2SHScriptSignature] = for {
    scriptPubKey <- pickRandomNonP2SHScriptPubKey
    scriptSig <- pickCorrespondingScriptSignature(scriptPubKey)
    p2shScriptSig = P2SHScriptSignature(scriptSig, scriptPubKey)
  } yield p2shScriptSig

  def p2pkScriptPubKey : Gen[P2PKScriptPubKey] = for {
    pubKey <- CryptoGenerators.publicKey
  } yield P2PKScriptPubKey(pubKey)

  def p2pkhScriptPubKey : Gen[P2PKHScriptPubKey] = for {
    pubKey <- CryptoGenerators.publicKey
  } yield P2PKHScriptPubKey(pubKey)

  def cltvScriptPubKey : Gen[CLTVScriptPubKey] = for {
    num <- NumberGenerator.scriptNumbers
    (cltv, privKeys, num) <- cltvScriptPubKey(num)
  } yield cltv

  def cltvScriptPubKey(num : ScriptNumber) : Gen[(CLTVScriptPubKey, ECPrivateKey, ScriptNumber)] = for {
    privKey <- CryptoGenerators.privateKey
    scriptPubKey <- pickRandomNonLockTimeScriptPubKey(privKey.publicKey)
  } yield {
    val cltv = CLTVScriptPubKey(num, scriptPubKey)
    (cltv, privKey, num)
  }

  def csvScriptPubKey : Gen[CSVScriptPubKey] = for {
    privKey <- CryptoGenerators.privateKey
    scriptPubKey <- pickRandomNonLockTimeScriptPubKey(privKey.publicKey)
    num <- NumberGenerator.scriptNumbers
  } yield CSVScriptPubKey(num, scriptPubKey)

  def multiSigScriptPubKey : Gen[MultiSignatureScriptPubKey] = {
    val pubKeys : Gen[(Int, Seq[ECPublicKey])] = for {
      numKeys <- Gen.choose(0,ScriptSettings.maxPublicKeysPerMultiSig)
      requiredSigs <- Gen.choose(0,numKeys)
    } yield (requiredSigs, for {
      _ <- 0 until numKeys
      pubKey = ECPublicKey()
    } yield pubKey)

    val multiSignatureScriptPubKey = pubKeys.map {
      case (requiredSigs, pubKeys) => MultiSignatureScriptPubKey(requiredSigs,pubKeys)
    }
    multiSignatureScriptPubKey
  }

  def p2shScriptPubKey : Gen[P2SHScriptPubKey] = for {
    randomScriptPubKey <- pickRandomNonP2SHScriptPubKey
  } yield P2SHScriptPubKey(randomScriptPubKey)

  def emptyScriptPubKey = p2pkScriptPubKey.map(_ => EmptyScriptPubKey)

  def pickRandomNonP2SHScriptPubKey : Gen[ScriptPubKey] = {
    val randomNum = (scala.util.Random.nextInt() % 5).abs
    if (randomNum == 0) p2pkScriptPubKey
    else if (randomNum == 1) p2pkhScriptPubKey
    else if (randomNum == 2) cltvScriptPubKey
    else if (randomNum == 3) csvScriptPubKey
    else multiSigScriptPubKey
  }

  def pickRandomNonLockTimeScriptPubKey(pubKey : ECPublicKey) : Gen[ScriptPubKey] = {
    val randomNum = (scala.util.Random.nextInt() % 3).abs
    if (randomNum == 0) P2PKScriptPubKey(pubKey)
    else if (randomNum == 1) P2PKHScriptPubKey(pubKey)
    else P2PKHScriptPubKey(pubKey)
  }

  def pickRandomNonLockTimeScriptPubKey : Gen[(ScriptPubKey, Seq[ECPrivateKey])] = {
    val num = (scala.util.Random.nextInt() % 5).abs
    val isMulti : Boolean = if (num == 4) true else false
      if (isMulti)  {
        for {
          (privKeys, requiredSigs)  <- CryptoGenerators.privateKeySeqWithRequiredSigs
          pubKeys = privKeys.map(_.publicKey)
        } yield (MultiSignatureScriptPubKey(requiredSigs, pubKeys), privKeys)
      } else {
        for {
          privKey <- CryptoGenerators.privateKey
          scriptPubKey <- pickRandomNonLockTimeScriptPubKey(privKey.publicKey)
        } yield (scriptPubKey, Seq(privKey))
      }
  }

  /**
    * Generates an arbitrary [[ScriptPubKey]]
    *
    * @return
    */
  def scriptPubKey : Gen[ScriptPubKey] = {
    val randomNum = (scala.util.Random.nextInt() % 7).abs
    if (randomNum == 0) p2pkScriptPubKey
    else if (randomNum == 1) p2pkhScriptPubKey
    else if (randomNum == 2) multiSigScriptPubKey
    else if (randomNum == 3) emptyScriptPubKey
    else if (randomNum == 4) cltvScriptPubKey
    else if (randomNum == 5) csvScriptPubKey
    else p2shScriptPubKey
  }

  /**
    * Generates an arbitrary [[ScriptSignature]]
    * @return
    */
  def scriptSignature : Gen[ScriptSignature] = {
    val randomNum = (scala.util.Random.nextInt() % 5).abs
    if (randomNum == 0) p2pkScriptSignature
    else if (randomNum == 1) p2pkhScriptSignature
    else if (randomNum == 2) multiSignatureScriptSignature
    else if (randomNum == 3) emptyScriptSignature
    else p2shScriptSignature
  }

  /**
    * Generates a [[ScriptSignature]] corresponding to the type of [[ScriptPubKey]] given.
    * Note: Does NOT generate a correct/valid signature
    * @param scriptPubKey
    * @return
    */
  private def pickCorrespondingScriptSignature(scriptPubKey : ScriptPubKey): Gen[ScriptSignature] = scriptPubKey match {
    case p2pk : P2PKScriptPubKey => p2pkScriptSignature
    case p2pkh : P2PKHScriptPubKey => p2pkhScriptSignature
    case multisig : MultiSignatureScriptPubKey => multiSignatureScriptSignature
    case EmptyScriptPubKey => emptyScriptSignature
    case cltv : CLTVScriptPubKey => pickCorrespondingScriptSignature(cltv.scriptPubKeyAfterCLTV)
    case csv : CSVScriptPubKey => pickCorrespondingScriptSignature(csv.scriptPubKeyAfterCSV)
    case x @ (_: P2SHScriptPubKey | _: NonStandardScriptPubKey) =>
      throw new IllegalArgumentException("Cannot pick for p2sh script pubkey, " +
        "non standard script pubkey, got: " + x)
  }

  /**
    * Generates a signed [[P2PKScriptSignature]] that spends the [[P2PKScriptPubKey]] correctly
    * @return the signed [[P2PKScriptSignature]], the [[P2PKScriptPubKey]] it spends, and the
    *         [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedP2PKScriptSignature: Gen[(P2PKScriptSignature,P2PKScriptPubKey,ECPrivateKey)] = for {
    privateKey <- CryptoGenerators.privateKey
  } yield {
    val publicKey = privateKey.publicKey
    val scriptPubKey = P2PKScriptPubKey(publicKey)
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    val scriptSig = P2PKScriptSignature(EmptyDigitalSignature)
    val (spendingTx, inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx, scriptSig, outputIndex)
    val txSignatureComponent = TransactionSignatureComponent(spendingTx, inputIndex, scriptPubKey, Policy.standardScriptVerifyFlags)
    val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privateKey, SIGHASH_ALL.defaultValue)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = P2PKScriptSignature(txSignature)
    (signedScriptSig,scriptPubKey,privateKey)
  }

  /**
    * Generates a signed [[P2PKHScriptSignature]] that spends the [[P2PKHScriptPubKey]] correctly
    * @return the signed [[P2PKHScriptSignature]], the [[P2PKHScriptPubKey]] it spends, and the
    *         [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedP2PKHScriptSignature: Gen[(P2PKHScriptSignature, P2PKHScriptPubKey, ECPrivateKey)] = for {
    privateKey <- CryptoGenerators.privateKey
  } yield {
    val publicKey = privateKey.publicKey
    val scriptPubKey = P2PKHScriptPubKey(publicKey)
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    val scriptSig = P2PKHScriptSignature(EmptyDigitalSignature,publicKey)
    val (spendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx,scriptSig,outputIndex)
    val txSignatureComponent = TransactionSignatureComponent(spendingTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
    val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent,privateKey, SIGHASH_ALL.defaultValue)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = P2PKHScriptSignature(txSignature,publicKey)
    (signedScriptSig, scriptPubKey, privateKey)
  }

  /**
    * Generates a signed [[MultiSignatureScriptSignature]] that spends the [[MultiSignatureScriptPubKey]] correctly
    * @return the signed [[MultiSignatureScriptSignature]], the [[MultiSignatureScriptPubKey]] it spends and the
    *         sequence of [[ECPrivateKey]] used to sign the scriptSig
    */
  def signedMultiSignatureScriptSignature: Gen[(MultiSignatureScriptSignature, MultiSignatureScriptPubKey, Seq[ECPrivateKey])] = for {
    (privateKeys, requiredSigs) <- CryptoGenerators.privateKeySeqWithRequiredSigs
  } yield {
    val publicKeys = privateKeys.map(_.publicKey)
    val scriptPubKey = MultiSignatureScriptPubKey(requiredSigs,publicKeys)
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(scriptPubKey)
    val emptyDigitalSignatures = publicKeys.map(_ => EmptyDigitalSignature)
    val scriptSig = MultiSignatureScriptSignature(emptyDigitalSignatures)
    val (spendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx,scriptSig,outputIndex)
    val txSignatureComponent = TransactionSignatureComponent(spendingTx,inputIndex,
      scriptPubKey,Policy.standardScriptVerifyFlags)

    val txSignatures = for {
      i <- 0 until requiredSigs
    } yield TransactionSignatureCreator.createSig(txSignatureComponent,privateKeys(i), SIGHASH_ALL.defaultValue)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = MultiSignatureScriptSignature(txSignatures)
    (signedScriptSig, scriptPubKey, privateKeys)
  }


  /**
    * Generates a signed [[P2SHScriptSignature]] that spends from a [[P2SHScriptPubKey]] correctly
    * @return the signed [[P2SHScriptSignature]], the [[P2SHScriptPubKey]] it spends, and the sequence of [[ECPrivateKey]]
    *         used to sign the scriptSig
    */
  def signedP2SHScriptSignature: Gen[(P2SHScriptSignature, P2SHScriptPubKey, Seq[ECPrivateKey])] = for {
    (scriptSig, redeemScript, privateKeys) <- chooseSignedScriptSig
  } yield {
    val p2SHScriptPubKey = P2SHScriptPubKey(redeemScript)
    val p2SHScriptSignature = P2SHScriptSignature(scriptSig,redeemScript)
    (p2SHScriptSignature, p2SHScriptPubKey, privateKeys)
  }

  /**
    * Generates a signed [[CLTVScriptSignature]] that spends from a [[CLTVScriptPubKey]] correctly
    * @return the signed [[CLTVScriptSignature]], the [[CLTVScriptPubKey]] it spends, and the sequences of [[ECPrivateKey]]
    *         used to sign the scriptSig
    */
  def signedCLTVScriptSignature(scriptNumber : ScriptNumber, lockTime : UInt32, sequence : UInt32) : Gen[(ScriptSignature, CLTVScriptPubKey, Seq[ECPrivateKey])] = for {
    (cltv, privKey, num) <- cltvScriptPubKey(scriptNumber)
  } yield {
    val (creditingTx, outputIndex) = TransactionGenerators.buildCreditingTransaction(cltv)
    val (unsignedSpendingTx, inputIndex) = {
      TransactionGenerators.buildSpendingTransaction(creditingTx, EmptyScriptSignature, outputIndex, lockTime, sequence)
    }
    val txSignatureComponent = TransactionSignatureComponent(unsignedSpendingTx, inputIndex, cltv, Policy.standardScriptVerifyFlags)
    val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privKey, SIGHASH_ALL.defaultValue)
    val signedScriptSig = CLTVScriptSignature(cltv, Seq(txSignature), Seq(privKey.publicKey))
    (signedScriptSig, cltv, Seq(privKey))
  }

  /**
    * This function chooses a random signed [[ScriptSignature]] that is NOT a [[P2SHScriptSignature]]
    * @return the signed [[ScriptSignature]], the [[ScriptPubKey]] it is spending,
    *         and the sequence of[[ECPrivateKey]] used to sign it
    */
  private def chooseSignedScriptSig: Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])] = {
    val scriptSig: Gen[Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])]] = for {
      num <- Gen.choose(0, 2)
    } yield {
      if (num == 0) packageToSequenceOfPrivateKeys(signedP2PKScriptSignature)
      else if (num == 1) packageToSequenceOfPrivateKeys(signedP2PKHScriptSignature)
      else signedMultiSignatureScriptSignature
    }
    //gets rid of Gen[Gen[...]]
    scriptSig.flatMap(g => g)
  }
  /**
    * Simply converts one private key in the generator to a sequence of private keys
 *
    * @param gen
    * @return
    */
  def packageToSequenceOfPrivateKeys(gen: Gen[(ScriptSignature, ScriptPubKey, ECPrivateKey)]): Gen[(ScriptSignature, ScriptPubKey, Seq[ECPrivateKey])] = for {
    (scriptSig, scriptPubKey, privateKey) <- gen
  } yield (scriptSig, scriptPubKey, Seq(privateKey))
}

object ScriptGenerators extends ScriptGenerators
