package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECDigitalSignature, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.script.ScriptSettings
import org.bitcoins.core.util.CryptoUtil
import org.scalacheck.Gen

/**
  * Created by chris on 6/22/16.
  */
trait CryptoGenerators {


  def privateKey : Gen[ECPrivateKey] = for {
    i <- Gen.choose(1,2)
  } yield ECPrivateKey()

  /**
    * Generate
    * @param num maximum number of keys to generate
    * @return
    */
  def privateKeySeq(num : Int): Gen[Seq[ECPrivateKey]] = Gen.listOfN(num,privateKey)

  /**
    * Generates a sequence of private keys, and determines an amount of 'required' private keys
    * that a transaction needs to be signed with
    * @param num
    * @return
    */
  def privateKeySeqWithRequiredSigs(num: Int): Gen[(Seq[ECPrivateKey], Int)] = {
    val privateKeys = privateKeySeq(num)
    for {
      keys <- privateKeys
      requiredSigs <- Gen.choose(0,keys.size-1)
    } yield (keys,requiredSigs)
  }

  def privateKeySeqWithRequiredSigs: Gen[(Seq[ECPrivateKey], Int)] = for {
    num <- Gen.choose(0,ScriptSettings.maxPublicKeysPerMultiSig)
    keysAndRequiredSigs <- privateKeySeqWithRequiredSigs(num)
  } yield keysAndRequiredSigs

  def publicKey : Gen[ECPublicKey] = for {
    privKey <- privateKey
  } yield privKey.publicKey

  def digitalSignatures : Gen[ECDigitalSignature] = for {
    privKey <- privateKey
    hexString <- StringGenerators.hexString
  } yield privKey.sign(hexString)

  def doubleSha256Digest : Gen[DoubleSha256Digest] = for {
    hex <- StringGenerators.hexString
    digest = CryptoUtil.doubleSHA256(hex)
  } yield digest

  def doubleSha256DigestSeq(num : Int): Gen[Seq[DoubleSha256Digest]] = Gen.listOfN(num,doubleSha256Digest)


}

object CryptoGenerators extends CryptoGenerators