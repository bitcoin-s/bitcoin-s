package org.bitcoins.crypto

import org.bitcoins.core.util.BytesUtil
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator}
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.scalacheck.Gen
import scodec.bits._

/** Created by chris on 1/26/16.
  */
class CryptoUtilTest extends BitcoinSUnitTest {

  "CryptoUtil" must "perform a SHA-1 hash" in {
    val hash = CryptoUtil.sha1(hex"")
    val expected = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    hash.hex must be(expected)
    hash.flip.flip.hex must be(expected)
  }

  it must "perform the correct RIPEMD160 on a string" in {
    val bytes = hex""
    val expectedDigest = "9c1185a5c5e9fc54612808977ee8f548b2258d31"
    CryptoUtil.ripeMd160(bytes).hex must be(expectedDigest)
    CryptoUtil.ripeMd160(bytes).flip.flip.hex must be(expectedDigest)
  }

  it must "perform a RIPEMD160 on a SHA256 hash to generate a bitcoin address" in {
    //https://bitcoin.stackexchange.com/questions/37040/ripemd160sha256publickey-where-am-i-going-wrong
    val bytes =
      hex"ea571f53cb3a9865d3dc74735e0c16643d319c6ad81e199b9c8408cecbcec7bb"
    val expected = "5238c71458e464d9ff90299abca4a1d7b9cb76ab"
    CryptoUtil.ripeMd160(bytes).hex must be(expected)
    CryptoUtil.ripeMd160(bytes).flip.flip.hex must be(expected)
  }

  it must "perform a single SHA256 hash on a byte vector" in {
    val bytes = hex""
    val expected =
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    CryptoUtil.sha256(bytes).hex must be(expected)
    CryptoUtil.sha256(bytes).hex must be(expected)
    CryptoUtil.sha256(bytes).flip.flip.hex must be(expected)
  }

  it must "perform a single SHA256 hash on a bit vector" in {
    val binary = bin"010001101110010001101110"
    val strBytes = BytesUtil.decodeHex(binary.toHex)

    val shaStrBytes = CryptoUtil.sha256(strBytes)
    val shaBinary = CryptoUtil.sha256(binary)
    val shaBytes = CryptoUtil.sha256(binary.toByteVector)

    shaStrBytes must be(shaBinary)
    shaBytes must be(shaBinary)
    shaBytes must be(shaStrBytes)
  }

  it must "perform a double SHA256 hash" in {
    val bytes = hex""
    val expected =
      "5df6e0e2761359d30a8275058e299fcc0381534545f55cf43e41983f5d4c9456"
    CryptoUtil.doubleSHA256(bytes).hex must be(expected)
    CryptoUtil.doubleSHA256(bytes).hex must be(expected)
    CryptoUtil.doubleSHA256(bytes).flip.flip.hex must be(expected)
  }

  it must "perform a double SHA256RIPEMD160 hash" in {
    val bytes = hex""
    val expected = "b472a266d0bd89c13706a4132ccfb16f7c3b9fcb"
    CryptoUtil.sha256Hash160(bytes).hex must be(expected)
    CryptoUtil.sha256Hash160(bytes).hex must be(expected)
    CryptoUtil.sha256Hash160(bytes).flip.flip.hex must be(expected)
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

  it must "compute tagged hashes correctly" in {
    forAll(NumberGenerator.bytevector) { bytes =>
      assert(
        CryptoUtil.sha256SchnorrChallenge(bytes) == CryptoUtil
          .taggedSha256(bytes, "BIP0340/challenge"))
      assert(
        CryptoUtil.sha256SchnorrNonce(bytes) == CryptoUtil
          .taggedSha256(bytes, "BIP0340/nonce"))
      assert(
        CryptoUtil.sha256SchnorrAuxRand(bytes) == CryptoUtil
          .taggedSha256(bytes, "BIP0340/aux"))
    }
  }

  // From https://github.com/dgarage/NDLC/blob/d816c0c517611b336f09ceaa43d400ecb5ab909b/NDLC.Tests/Data/normalization_tests.json
  it must "normalize and serialize strings correctly" in {
    val singletons = Vector("\u00c5", "\u212b", "\u0041\u030a")
    assert(
      singletons
        .map(CryptoUtil.normalize)
        .forall(_ == "\u00c5")
    )
    assert(
      CryptoUtil.serializeForHash("\u00c5") == ByteVector.fromValidHex("c385")
    )

    val canonicalComposites = Vector("\u00f4", "\u006f\u0302")
    assert(
      canonicalComposites
        .map(CryptoUtil.normalize)
        .forall(_ == "\u00f4")
    )
    assert(
      CryptoUtil.serializeForHash("\u00f4") == ByteVector.fromValidHex("c3b4")
    )

    val multipleCombiningMarks = Vector("\u1e69", "\u0073\u0323\u0307")
    assert(
      multipleCombiningMarks.map(CryptoUtil.normalize).forall(_ == "\u1e69")
    )
    assert(
      CryptoUtil.serializeForHash("\u1e69") == ByteVector.fromValidHex("e1b9a9")
    )

    val compatibilityComposite = "\ufb01"
    assert(
      CryptoUtil.serializeForHash(compatibilityComposite) == ByteVector
        .fromValidHex("efac81")
    )

    val nonComposite = "fi"
    assert(
      CryptoUtil.serializeForHash(nonComposite) == ByteVector.fromValidHex(
        "6669")
    )

    val accentString = "éléphant"
    assert(
      CryptoUtil.serializeForHash(accentString) == ByteVector.fromValidHex(
        "c3a96cc3a97068616e74")
    )
  }

  // From https://github.com/dgarage/NDLC/blob/d816c0c517611b336f09ceaa43d400ecb5ab909b/NDLC.Tests/Data/normalization_tests.json
  it must "sha256 unicode strings correctly" in {
    val singletons = Vector("\u00c5", "\u212b", "\u0041\u030a")
    assert(
      singletons
        .map(CryptoUtil.sha256)
        .forall(_ == Sha256Digest(
          "0a94dc9d420d1142d6b71de60f9bf7e2f345a4d62c9f141b091539769ddf3075"))
    )

    val canonicalComposites = Vector("\u00f4", "\u006f\u0302")
    assert(
      canonicalComposites
        .map(CryptoUtil.sha256)
        .forall(_ == Sha256Digest(
          "cc912dbca598fd80ca7f5d98ece5d846b447f4a9ae3f73c352e2687eb293eef5"))
    )

    val multipleCombiningMarks = Vector("\u1e69", "\u0073\u0323\u0307")
    assert(
      multipleCombiningMarks
        .map(CryptoUtil.sha256)
        .forall(_ == Sha256Digest(
          "ceca1ea456e95ee498463622915209bb08a018e8ee9741b46b64ef1a08fb56ab"))
    )

    val compatibilityComposite = "\ufb01"
    assert(
      CryptoUtil.sha256(compatibilityComposite) == Sha256Digest(
        "b6554cce8a93f1c8818280e2a768116a79216ad5501a85357d233409db87d340"))

    val nonComposite = "fi"
    assert(
      CryptoUtil.sha256(nonComposite) == Sha256Digest(
        "b4bdc848109722a383d0a972c6eb859f2abd29565b8c4cc7199e7c9eb708f1b7"))

    val accentString = "éléphant"
    assert(
      CryptoUtil.sha256(accentString) == Sha256Digest(
        "c941ae685f62cbe7bb47d0791af7154788fd9e873e5c57fd2449d1454ed5b16f"))
  }

  it must "encode strings correctly when hashing" in {
    forAll(Gen.alphaStr) { str =>
      val serialized = CryptoUtil.serializeForHash(str)
      val strHashFuncs: Vector[String => HashDigest] =
        Vector(
          CryptoUtil.sha256Hash160,
          CryptoUtil.sha256,
          CryptoUtil.taggedSha256(_, "test"),
          CryptoUtil.sha1,
          CryptoUtil.ripeMd160
        )
      val byteHashFuncs: Vector[ByteVector => HashDigest] =
        Vector(
          CryptoUtil.sha256Hash160,
          CryptoUtil.sha256,
          CryptoUtil.taggedSha256(_, "test"),
          CryptoUtil.sha1,
          CryptoUtil.ripeMd160
        )
      val hashFuncs = strHashFuncs.zip(byteHashFuncs)
      assert(hashFuncs.forall { case (strHash, byteHash) =>
        strHash(str) == byteHash(serialized)
      })
    }
  }
}
