package org.bitcoins.testkit.core.gen

import org.bitcoins.core.crypto._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil
import org.scalacheck.Gen
import scodec.bits.BitVector

/**
  * Created by chris on 6/22/16.
  */
sealed abstract class CryptoGenerators {

  object entropy {

    /**
      * Generates 128 bits of entropy
      */
    def bits128: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy128Bits)

    /**
      * Generates 160 bits of entropy
      */
    def bits160: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy160Bits)

    /**
      * Generates 192 bits of entropy
      */
    def bits192: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy192Bits)

    /**
      * Generates 224 bits of entropy
      */
    def bits224: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy224Bits)

    /**
      * Generates 256 bits of entropy
      */
    def bits256: Gen[BitVector] = Gen.delay(MnemonicCode.getEntropy256Bits)

    /**
      * Generates either 128, 160, 192, 224 or 256 of bits of entropy
      */
    def any: Gen[BitVector] =
      Gen.oneOf(bits128, bits160, bits192, bits224, bits256)
  }

  def mnemonicCode128Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits128
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode160Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits160
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode192Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits192
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode224Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits224
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode256Bits: Gen[MnemonicCode] =
    for {
      entropy <- entropy.bits256
    } yield MnemonicCode.fromEntropy(entropy)

  def mnemonicCode: Gen[MnemonicCode] =
    Gen.oneOf(
      mnemonicCode128Bits,
      mnemonicCode160Bits,
      mnemonicCode192Bits,
      mnemonicCode224Bits,
      mnemonicCode256Bits
    )

  /**
    * Generates a BIP39 valid mnemonic
    * phrase
    */
  def mnemonicPhrase: Gen[Vector[String]] =
    for {
      code <- mnemonicCode
    } yield code.words

  /**
    * Generates a valid BIP39 seed from
    * an mnemonic with no password
    */
  def bip39SeedNoPassword: Gen[BIP39Seed] =
    for {
      code <- mnemonicCode
    } yield BIP39Seed.fromMnemonic(code)

  /**
    * Generates a valid BIP39 seed from
    * an mnemonic with a random password
    */
  def bip39SeedWithPassword: Gen[BIP39Seed] =
    for {
      code <- mnemonicCode
      pass <- Gen.alphaStr
    } yield BIP39Seed.fromMnemonic(code, pass)

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
      key <- privateKey
      digest = CryptoUtil.doubleSHA256(key.bytes)
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

  def extVersion: Gen[ExtKeyVersion] = {
    Gen.oneOf(ExtKeyVersion.all)
  }

  /** Generates an [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]] */
  def extPrivateKey: Gen[ExtPrivateKey] = {
    for {
      version <- Gen.oneOf(ExtKeyVersion.allPrivs)
      ext = ExtPrivateKey(version)
    } yield ext
  }

  def extPublicKey: Gen[ExtPublicKey] = extPrivateKey.map(_.extPublicKey)

  def extKey: Gen[ExtKey] = Gen.oneOf(extPrivateKey, extPublicKey)

  def aesKey128Bit: Gen[AesKey] = Gen.delay(AesKey.get128Bit())
  def aesKey192Bit: Gen[AesKey] = Gen.delay(AesKey.get192Bit())
  def aesKey256Bit: Gen[AesKey] = Gen.delay(AesKey.get256Bit())

  def aesKey: Gen[AesKey] =
    Gen.oneOf(aesKey128Bit, aesKey192Bit, aesKey256Bit)

  def aesPassword: Gen[AesPassword] =
    Gen.alphaStr.suchThat(_.nonEmpty).map(AesPassword.fromNonEmptyString(_))

  def aesIV: Gen[AesIV] = Gen.delay(AesIV.random)

  def aesEncryptedData: Gen[AesEncryptedData] =
    for {
      cipher <- NumberGenerator.bytevector.suchThat(_.nonEmpty)
      iv <- aesIV
    } yield AesEncryptedData(cipherText = cipher, iv)
}

object CryptoGenerators extends CryptoGenerators
