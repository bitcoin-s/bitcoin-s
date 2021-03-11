package org.bitcoins.crypto

import org.bitcoins.crypto
import org.scalacheck.Gen
import scodec.bits.ByteVector

/** Created by chris on 6/22/16.
  */
sealed abstract class CryptoGenerators {

  def privateKey: Gen[ECPrivateKey] = Gen.delay(ECPrivateKey())

  def fieldElement: Gen[FieldElement] = privateKey.map(_.fieldElement)

  def smallFieldElement: Gen[FieldElement] =
    NumberGenerator
      .bytevector(30)
      .map(bytes => FieldElement(ByteVector.fill(2)(0) ++ bytes))

  def reallySmallFieldElement: Gen[FieldElement] =
    NumberGenerator
      .bytevector(15)
      .map(bytes => FieldElement(ByteVector.fill(17)(0) ++ bytes))

  def largeFieldElement: Gen[FieldElement] =
    NumberGenerator
      .bytevector(30)
      .map(bytes => FieldElement(ByteVector.fill(2)(Byte.MinValue) ++ bytes))

  def nonZeroFieldElement: Gen[FieldElement] =
    nonZeroPrivKey.map(_.fieldElement)

  /** Generates a random non-zero private key */
  def nonZeroPrivKey: Gen[ECPrivateKey] =
    privateKey.filter(_.bytes.toArray.exists(_ != 0.toByte))

  def schnorrNonce: Gen[SchnorrNonce] =
    nonZeroPrivKey.map(_.publicKey.bytes.tail).map(SchnorrNonce.fromBytes)

  def schnorrPublicKey: Gen[SchnorrPublicKey] =
    publicKey.map(_.schnorrPublicKey)

  /** Generate a sequence of private keys
    * @param num maximum number of keys to generate
    * @return
    */
  def privateKeySeq(num: Int): Gen[Seq[ECPrivateKey]] =
    Gen.listOfN(num, privateKey)

  /** Generates a sequence of private keys, and determines an amount of 'required' private keys
    * that a transaction needs to be signed with
    * @param num the maximum number of keys to generate
    * @return
    */
  def privateKeySeqWithRequiredSigs(num: Int): Gen[(Seq[ECPrivateKey], Int)] = {
    if (num <= 0) {
      Gen.const((Nil, 0))
    } else {
      val privateKeys = privateKeySeq(num)
      for {
        keys <- privateKeys
        requiredSigs <- Gen.choose(0, keys.size - 1)
      } yield (keys, requiredSigs)
    }
  }

  /** Generates a random number of private keys less than 15.
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

  def schnorrDigitalSignature: Gen[SchnorrDigitalSignature] = {
    for {
      privKey <- privateKey
      hash <- CryptoGenerators.doubleSha256Digest
    } yield privKey.schnorrSign(hash.bytes)
  }

  def adaptorSignature: Gen[ECAdaptorSignature] = {
    for {
      tweakedNonce <- publicKey
      untweakedNonce <- publicKey
      adaptedS <- fieldElement
      proofS <- fieldElement
      proofE <- fieldElement
    } yield {
      ECAdaptorSignature(tweakedNonce, adaptedS, untweakedNonce, proofS, proofE)
    }
  }

  def sha256Digest: Gen[Sha256Digest] =
    for {
      bytes <- NumberGenerator.bytevector
      digest = CryptoUtil.sha256(bytes)
    } yield digest

  def sha256DigestBE: Gen[Sha256DigestBE] = {
    sha256Digest.map(_.flip)
  }

  /** Generates a random [[DoubleSha256Digest DoubleSha256Digest]] */
  def doubleSha256Digest: Gen[DoubleSha256Digest] =
    for {
      key <- privateKey
      digest = CryptoUtil.doubleSHA256(key.bytes)
    } yield digest

  def doubleSha256DigestBE: Gen[DoubleSha256DigestBE] = {
    doubleSha256Digest.map(_.flip)
  }

  /** Generates a sequence of [[DoubleSha256Digest DoubleSha256Digest]]
    * @param num the number of digets to generate
    * @return
    */
  def doubleSha256DigestSeq(num: Int): Gen[Seq[DoubleSha256Digest]] =
    Gen.listOfN(num, doubleSha256Digest)

  /** Generates a random [[Sha256Hash160Digest Sha256Hash160Digest]] */
  def sha256Hash160Digest: Gen[Sha256Hash160Digest] =
    for {
      pubKey <- publicKey
      hash = CryptoUtil.sha256Hash160(pubKey.bytes)
    } yield hash

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
    } yield crypto.AesEncryptedData(cipherText = cipher, iv)

  def genKey: Gen[SipHashKey] =
    Gen
      .listOfN(16, NumberGenerator.byte)
      .map(ByteVector(_))
      .map(SipHashKey(_))
}

object CryptoGenerators extends CryptoGenerators
