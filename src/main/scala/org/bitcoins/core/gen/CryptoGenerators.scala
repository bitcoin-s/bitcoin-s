package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{DoubleSha256Digest, ECDigitalSignature, ECPrivateKey, ECPublicKey}
import org.bitcoins.core.util.CryptoUtil
import org.scalacheck.Gen

/**
  * Created by chris on 6/22/16.
  */
trait CryptoGenerators {


  def privateKey : Gen[ECPrivateKey] = for {
    i <- Gen.choose(1,2)
  } yield ECPrivateKey()

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