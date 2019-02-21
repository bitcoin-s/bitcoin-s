package org.bitcoins.testkit.core.gen

import org.bitcoins.core.crypto._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil
import org.scalacheck.Gen

/**
  * Created by chris on 6/22/16.
  */
sealed abstract class CryptoGenerators {

  def privateKey: Gen[ECPrivateKey] = Gen.delay(ECPrivateKey())

  /**
    * Generate a sequence of private keys
    * @param num maximum number of keys to generate
    * @return
    */
  def privateKeySeq(num: Int): Gen[Seq[ECPrivateKey]] =
    Gen.listOfN(num, privateKey)

  /**
    * Generates a sequence of private keys, and determines an amount of 'required' private keys
    * that a transaction needs to be signed with
    * @param num the maximum number of keys to generate
    * @return
    */
  def privateKeySeqWithRequiredSigs(num: Int): Gen[(Seq[ECPrivateKey], Int)] = {
    if (num <= 0) {
      Gen.const(Nil, 0)
    } else {
      val privateKeys = privateKeySeq(num)
      for {
        keys <- privateKeys
        requiredSigs <- Gen.choose(0, keys.size - 1)
      } yield (keys, requiredSigs)
    }
  }

  /**
    * Generates a random number of private keys less than 15.
    * Also generates a random 'requiredSigs' number that a transaction needs to be signed with
    */
  def privateKeySeqWithRequiredSigs: Gen[(Seq[ECPrivateKey], Int)] =
    for {
      num <- Gen.choose(0, 15)
      keysAndRequiredSigs <- privateKeySeqWithRequiredSigs(num)
    } yield keysAndRequiredSigs

  /** A generator with 7 or less private keys -- useful for creating smaller scripts */
  def smallPrivateKeySeqWithRequiredSigs: Gen[(Seq[ECPrivateKey], Int)] =
    for {
      num <- Gen.choose(0, 7)
      keysAndRequiredSigs <- privateKeySeqWithRequiredSigs(num)
    } yield keysAndRequiredSigs

  /** Generates a random public key */
  def publicKey: Gen[ECPublicKey] =
    for {
      privKey <- privateKey
    } yield privKey.publicKey

  /** Generates a random digital signature */
  def digitalSignature: Gen[ECDigitalSignature] =
    for {
      privKey <- privateKey
      hash <- CryptoGenerators.doubleSha256Digest
    } yield privKey.sign(hash)

  def sha256Digest: Gen[Sha256Digest] =
    for {
      hex <- StringGenerators.hexString
      digest = CryptoUtil.sha256(hex)
    } yield digest

  /** Generates a random [[org.bitcoins.core.crypto.DoubleSha256Digest DoubleSha256Digest]] */
  def doubleSha256Digest: Gen[DoubleSha256Digest] =
    for {
      hex <- StringGenerators.hexString
      digest = CryptoUtil.doubleSHA256(hex)
    } yield digest

  /**
    * Generates a sequence of [[org.bitcoins.core.crypto.DoubleSha256Digest DoubleSha256Digest]]
    * @param num the number of digets to generate
    * @return
    */
  def doubleSha256DigestSeq(num: Int): Gen[Seq[DoubleSha256Digest]] =
    Gen.listOfN(num, doubleSha256Digest)

  /** Generates a random [[org.bitcoins.core.crypto.Sha256Hash160Digest Sha256Hash160Digest]] */
  def sha256Hash160Digest: Gen[Sha256Hash160Digest] =
    for {
      pubKey <- publicKey
      hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    } yield hash

  /** Generates a random [[org.bitcoins.core.script.crypto.HashType HashType]] */
  def hashType: Gen[HashType] =
    Gen.oneOf(
      HashType.sigHashAll,
      HashType.sigHashNone,
      HashType.sigHashSingle,
      HashType.sigHashAnyoneCanPay,
      HashType.sigHashSingleAnyoneCanPay,
      HashType.sigHashNoneAnyoneCanPay,
      HashType.sigHashAllAnyoneCanPay
    )

  def extVersion: Gen[ExtKeyVersion] =
    Gen.oneOf(MainNetPriv, MainNetPub, TestNet3Priv, TestNet3Pub)

  /** Generates an [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]] */
  def extPrivateKey: Gen[ExtPrivateKey] =
    for {
      version <- Gen.oneOf(MainNetPriv, TestNet3Priv)
      ext = ExtPrivateKey(version)
    } yield ext

  def extPublicKey: Gen[ExtPublicKey] = extPrivateKey.map(_.extPublicKey)

  def extKey: Gen[ExtKey] = Gen.oneOf(extPrivateKey, extPublicKey)

}

object CryptoGenerators extends CryptoGenerators
