package org.bitcoins.core.util

import org.bitcoins.core.gen.CryptoGenerators
import org.scalatest.prop.{Configuration, PropertyChecks}
import org.scalatest.{FlatSpec, MustMatchers}
import org.slf4j.LoggerFactory

/**
  * Created by chris on 1/26/16.
  */
class CryptoUtilTest extends FlatSpec with MustMatchers with PropertyChecks {
  private val logger = LoggerFactory.getLogger(this.getClass)

  "CryptoUtil" must "perform a SHA-1 hash" in {
    val hash = CryptoUtil.sha1("")
    val expected = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    hash.hex must be(expected)
    hash.flip.flip.hex must be(expected)
  }

  it must "perform the correct RIPEMD160 on a string" in {
    val str = ""
    val expectedDigest = "9c1185a5c5e9fc54612808977ee8f548b2258d31"
    CryptoUtil.ripeMd160(str).hex must be(expectedDigest)
    CryptoUtil.ripeMd160(str).flip.flip.hex must be(expectedDigest)
  }

  it must "perform a RIPEMD160 on a SHA256 hash to generate a bitcoin address" in {
    //https://bitcoin.stackexchange.com/questions/37040/ripemd160sha256publickey-where-am-i-going-wrong
    val str = "ea571f53cb3a9865d3dc74735e0c16643d319c6ad81e199b9c8408cecbcec7bb"
    val expected = "5238c71458e464d9ff90299abca4a1d7b9cb76ab"
    CryptoUtil.ripeMd160(str).hex must be(expected)
    CryptoUtil.ripeMd160(str).flip.flip.hex must be(expected)
  }

  it must "perform a single SHA256 hash" in {
    val hex = ""
    val strBytes = BitcoinSUtil.decodeHex(hex)
    val expected =
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    CryptoUtil.sha256(strBytes).hex must be(expected)
    CryptoUtil.sha256(hex).hex must be(expected)
    CryptoUtil.sha256(hex).flip.flip.hex must be(expected)
  }

  it must "perform a double SHA256 hash" in {
    val hex = ""
    val strBytes = BitcoinSUtil.decodeHex(hex)
    val expected =
      "5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456"
    CryptoUtil.doubleSHA256(strBytes).hex must be(expected)
    CryptoUtil.doubleSHA256(hex).hex must be(expected)
    CryptoUtil.doubleSHA256(hex).flip.flip.hex must be(expected)
  }

  it must "perform a double SHA256RIPEMD160 hash" in {
    val hex = ""
    val strBytes = BitcoinSUtil.decodeHex(hex)
    val expected = "b472a266d0bd89c13706a4132ccfb16f7c3b9fcb"
    CryptoUtil.sha256Hash160(strBytes).hex must be(expected)
    CryptoUtil.sha256Hash160(hex).hex must be(expected)
    CryptoUtil.sha256Hash160(hex).flip.flip.hex must be(expected)
  }

  it must "recover the 2 public keys from a digital signature" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.sha256Digest) {
      case (privKey, hash) =>
        val pubKey = privKey.publicKey
        val message = hash.bytes
        val sig = privKey.sign(message)
        val (recovPub1, recovPub2) = CryptoUtil.recoverPublicKey(sig, message)
        assert(recovPub1 == pubKey || recovPub2 == pubKey)
    }
  }

  it must "be able to recover and verify a siganture for a message" in {
    forAll(CryptoGenerators.privateKey, CryptoGenerators.sha256Digest) {
      (privKey, hash) =>
        val message = hash.bytes
        val sig = privKey.sign(message)
        val (recovPub1, recovPub2) = CryptoUtil.recoverPublicKey(sig, message)
        assert(recovPub1.verify(message, sig) && recovPub2.verify(message, sig))
    }
  }

}
