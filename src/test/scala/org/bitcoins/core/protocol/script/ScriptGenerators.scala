package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{CryptoGenerators, ECDigitalSignature, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.script.ScriptSettings
import org.bitcoins.core.util.{BitcoinSLogger, StringGenerators}
import org.scalacheck.Gen

/**
  * Created by chris on 6/22/16.
  */
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

  def multiSigScriptPubKey : Gen[MultiSignatureScriptPubKey] = {
    val pubKeys : Gen[(Int, Seq[ECPublicKey])] = for {
      numKeys <- Gen.choose(0,ScriptSettings.maxPublicKeysPerMultiSig)
      requiredSigs <- Gen.choose(0,numKeys)
    } yield (requiredSigs, for {
      _ <- 0 until numKeys
      pubKey = ECPublicKey()
    } yield pubKey)

    val multiSignatureScriptPubKey = pubKeys.map {
      case (requiredSigs, pubKeys) =>
        logger.info("Required sigs: " + requiredSigs)
        logger.info("Pubkeys length: " +pubKeys.length)
        MultiSignatureScriptPubKey(requiredSigs,pubKeys)
    }
    multiSignatureScriptPubKey
  }

  def p2shScriptPubKey : Gen[P2SHScriptPubKey] = for {
    randomScriptPubKey <- pickRandomNonP2SHScriptPubKey
  } yield P2SHScriptPubKey(randomScriptPubKey)


  private def pickRandomNonP2SHScriptPubKey : Gen[ScriptPubKey] = {
    val randomNum = scala.util.Random.nextInt() % 3
    if (randomNum == 0) p2pkhScriptPubKey
    else if (randomNum == 1) p2pkhScriptPubKey
    else multiSigScriptPubKey
  }

  private def pickCorrespondingScriptSignature(scriptPubKey : ScriptPubKey): Gen[ScriptSignature] = scriptPubKey match {
    case p2pk : P2PKScriptPubKey => p2pkScriptSignature
    case p2pkh : P2PKHScriptPubKey => p2pkhScriptSignature
    case multisig : MultiSignatureScriptPubKey => multiSignatureScriptSignature
    case x : ScriptPubKey =>
      throw new IllegalArgumentException("Cannot pick for p2sh script pubkey, " +
        "non standard script pubkey or Empty script pubKey, got: " + x)
  }

}

object ScriptGenerators extends ScriptGenerators
