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

}

object ScriptGenerators extends ScriptGenerators
