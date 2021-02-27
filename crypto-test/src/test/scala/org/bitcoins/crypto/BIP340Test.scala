package org.bitcoins.crypto

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.util.{Failure, Success, Try}

/** Tests from https://github.com/sipa/bips/blob/bip-taproot/bip-0340/test-vectors.csv */
class BIP340Test extends BitcoinSUnitTest {
  behavior of "Schnorr Signing"

  def testSign(
      index: Int,
      secKey: ECPrivateKey,
      auxRand: ByteVector,
      msg: ByteVector,
      expectedSig: SchnorrDigitalSignature): Assertion = {
    val secpSig = secKey.schnorrSign(msg, auxRand)
    val bouncyCastleSig =
      BouncycastleCryptoRuntime.schnorrSign(msg, secKey, auxRand)
    assert(secpSig == expectedSig,
           s"Test $index failed signing for libsecp256k1")
    assert(bouncyCastleSig == expectedSig,
           s"Test $index failed signing for Bouncy Castle")
    assert(bouncyCastleSig == secpSig)
  }

  def testVerify(
      index: Int,
      pubKey: SchnorrPublicKey,
      msg: ByteVector,
      sig: SchnorrDigitalSignature,
      expectedResult: Boolean,
      comment: String): Assertion = {
    val secpResult = pubKey.verify(msg, sig)
    val bouncyCastleResult =
      BouncycastleCryptoRuntime.schnorrVerify(msg, pubKey, sig)
    assert(secpResult == expectedResult,
           s"Test $index failed verification for libsecp256k1: $comment")
    assert(bouncyCastleResult == expectedResult,
           s"Test $index failed verification for Bouncy Castle: $comment")
    assert(bouncyCastleResult == secpResult)
  }

  def test(
      index: Int,
      secKeyOpt: Option[String],
      pubKey: String,
      auxRandOpt: Option[String],
      msg: String,
      sig: String,
      result: Boolean,
      comment: String): Assertion = {
    val pkT = Try(SchnorrPublicKey(pubKey))
    val msgBytes = ByteVector.fromHex(msg).get
    val schnorrSigT = Try(SchnorrDigitalSignature(sig))

    (pkT, schnorrSigT) match {
      case (Success(pk), Success(schnorrSig)) =>
        (secKeyOpt, auxRandOpt) match {
          case (Some(secKeyStr), Some(auxRandStr)) =>
            val secKey = ECPrivateKey(secKeyStr)
            assert(secKey.schnorrPublicKey == pk)
            val auxRand = ByteVector.fromHex(auxRandStr).get
            testSign(index, secKey, auxRand, msgBytes, schnorrSig)
          case _ => ()
        }

        testVerify(index, pk, msgBytes, schnorrSig, result, comment)
      case (Failure(_), _) |
          (_, Failure(_)) => // Must be verify only test resulting in false
        assert(secKeyOpt.isEmpty)
        assert(auxRandOpt.isEmpty)
        assert(!result, s"Test $index failed to parse signature: $comment")
    }
  }

  private def toOpt(str: String): Option[String] = {
    if (str.isEmpty) {
      None
    } else {
      Some(str)
    }
  }

  it must "pass the BIP 340 test-vectors with both secp256k1 bindings and bouncy castle" in {
    val bufferedSource =
      io.Source.fromURL(getClass.getResource("/bip340-test-vectors.csv"))
    try {
      val lines = bufferedSource.getLines()
      val _ = lines.next()
      for (line <- bufferedSource.getLines()) {
        val testVec = line.split(",").map(_.trim)
        val index = testVec.head.toInt
        val secKeyOpt = toOpt(testVec(1))
        val pubKey = testVec(2)
        val auxRandOpt = toOpt(testVec(3))
        val msg = testVec(4)
        val sig = testVec(5)
        val result = testVec(6).toBoolean
        val comment = if (testVec.length > 7) {
          testVec(7)
        } else {
          ""
        }

        test(index = index,
             secKeyOpt = secKeyOpt,
             pubKey = pubKey,
             auxRandOpt = auxRandOpt,
             msg = msg,
             sig = sig,
             result = result,
             comment = comment)
      }
    } finally {
      bufferedSource.close()
    }
  }
}
