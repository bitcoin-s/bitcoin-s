package org.bitcoins.crypto

import scodec.bits.ByteVector

/** Created by chris on 3/22/16.
  */
class ECDigitalSignatureTest extends BitcoinSCryptoTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  "ECDigitalSignature" must "say that empty signature is a valid DER encoded signature" in {
    val emptySiganture = ECDigitalSignature(ByteVector.empty)
    emptySiganture.isDEREncoded must be(true)

  }

  it must "say that a signature taken from a p2sh transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature(
      "304402205b7d2c2f177ae76cfbbf14d589c113b0b35db753d305d5562dd0b61cbf366cfb02202e56f93c4f08a27f986cd424ffc48a462c3202c4902104d4d0ff98ed28f4bf80"
    )
    signature.isDEREncoded must be(true)
  }

  it must "say that signature taken from a p2pkh transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature(
      "3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac"
    )
    signature.isDEREncoded must be(true)
  }

  it must "say that a signature taken from a p2pk transaction is a valid DER encoded signature" in {
    val signature = ECDigitalSignature(
      "304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b7420"
    )
    signature.isDEREncoded must be(true)
  }

  it must "say that the empty digital signatures r,s values are both 0" in {
    ECDigitalSignature.empty.r must be(0)
    ECDigitalSignature.empty.s must be(0)
  }

  it must "create an empty digital signature when given 0 in hex or byte format" in {
    val hex = ECDigitalSignature("00")
    val byte = ECDigitalSignature(ByteVector.low(1))
    val emptySignature = ECDigitalSignature("")
    byte must be(emptySignature)
    hex must be(emptySignature)
  }

  it must "not treat garbage bytes that silently decode to r=s=0 as a valid low-S or hash-typed signature" in {
    // ECDigitalSignature.fromBytes/apply itself must stay lenient and NOT
    // throw: it is called directly on untrusted, attacker-controlled
    // scriptSig bytes in consensus-critical script execution (e.g.
    // CryptoInterpreter.opCheckSig/opCheckMultiSig) with no Try wrapper, so
    // a malformed signature must fail verification cleanly rather than
    // crash script validation with an uncaught exception. Instead, the
    // derived accessors that previously silently treated an undecodable
    // signature as if it validly decoded to r=s=0 (isLowS, hashTypeOpt)
    // must correctly report that the bytes are not a valid DER signature.
    val garbageShort = ByteVector.fromValidHex("deadbeef")
    val garbageLong = ByteVector.fromValidHex(
      "0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f20")

    // must not throw -- garbage bytes are still representable so callers
    // like the script interpreter can cleanly fail signature checks
    val sigShort = ECDigitalSignature.fromBytes(garbageShort)
    val sigLong = ECDigitalSignature.fromBytes(garbageLong)

    // r=s=0 is trivially "low S", but undecodable garbage must not be
    // reported as having a low (i.e. validly encoded) S value
    DERSignatureUtil.isLowS(sigShort) must be(false)
    DERSignatureUtil.isLowS(sigLong) must be(false)

    // there's no reliable way to locate a trailing hash type byte without a
    // validly decoded signature
    sigShort.hashTypeOpt must be(None)
    sigLong.hashTypeOpt must be(None)
  }

  it must "must be der encoded" in {
    forAll(CryptoGenerators.digitalSignature) { signature =>
      assert(signature.isDEREncoded)
    }
  }

  it must "must have a low s" in {
    forAll(CryptoGenerators.digitalSignature) { signature =>
      assert(DERSignatureUtil.isLowS(signature))
    }
  }

  it must "must create and verify a digital signature" in {
    forAll(CryptoGenerators.doubleSha256Digest, CryptoGenerators.privateKey) {
      case (hash, key) =>
        val sig = key.sign(hash)
        assert(key.publicKey.verify(hash, sig))
    }
  }

  it must "must not reuse r values" in {
    forAll(
      CryptoGenerators.privateKey,
      CryptoGenerators.doubleSha256Digest,
      CryptoGenerators.doubleSha256Digest
    ) { case (key, hash1, hash2) =>
      val sig1 = key.sign(hash1)
      val sig2 = key.sign(hash2)
      assert(sig1.r != sig2.r)
    }
  }

  it must "must have serialization symmetry with r,s" in {
    forAll(CryptoGenerators.digitalSignature) { case sig: ECDigitalSignature =>
      val sig2 = ECDigitalSignature.fromRS(sig.r, sig.s)

      assert(sig2 == sig)
      assert(sig2.r == sig.r)
      assert(sig2.s == sig.s)
    }
  }

  it must "must have serialization symmetry toRawRS & fromRS" in {
    forAll(CryptoGenerators.digitalSignature) { case sig: ECDigitalSignature =>
      val raw = sig.toRawRS
      assert(ECDigitalSignature.fromRS(raw) == sig)
    }
  }

  it must "fail cleanly with an IllegalArgumentException when the sighash byte is absent" in {
    // fromFrontOfBytesWithSigHash previously threw NoSuchElementException
    // (from `.head` on an empty ByteVector) when the input had no sighash
    // byte after the DER signature. Correct behavior: a clean, documented
    // failure such as IllegalArgumentException.
    val rHex =
      "4c2dd8a9b6f8d425fcd8ee9a20ac73b619906a6367eac6cb93e70375225ec016"
    val sHex =
      "356878eff111ff3663d7e6bf08947f94443845e0dcc54961664d922f7660b80c"
    val strictDerNoSigHash: ByteVector =
      ByteVector.fromValidHex("3044" + "0220" + rHex + "0220" + sHex)

    intercept[IllegalArgumentException] {
      ECDigitalSignature.fromFrontOfBytesWithSigHash(strictDerNoSigHash)
    }
  }

  it must "not view an incorrectly encoded sig_hash byte as strictly encoded" in {
    forAll(CryptoGenerators.digitalSignature, CryptoGenerators.hashType) {
      case (sig, hashType) =>
        val bigSigHash = ByteVector.fromInt(i = hashType.num, size = 4)
        assert(sig.hashTypeOpt.isEmpty)

        val sigWithBigSigHash = ECDigitalSignature(sig.bytes.++(bigSigHash))
        assert(sigWithBigSigHash.hashTypeOpt.contains(hashType))
        assert(!sigWithBigSigHash.isStrictEncoded)
    }
  }

  it must "correctly append and find HashTypes" in {
    forAll(CryptoGenerators.digitalSignature, CryptoGenerators.hashType) {
      case (sig, hashType) =>
        assert(sig.hashTypeOpt.isEmpty)
        val sigWithHashType = sig.appendHashType(hashType)
        assert(sigWithHashType.hashTypeOpt.contains(hashType))
    }
  }

  it must "not append a HashType if one is already there" in {
    forAll(
      CryptoGenerators.digitalSignature,
      CryptoGenerators.hashType,
      CryptoGenerators.hashType
    ) { case (sig, hashType, hashType2) =>
      val sigWithHashType = sig.appendHashType(hashType)
      assertThrows[IllegalArgumentException](
        sigWithHashType.appendHashType(hashType2)
      )
    }
  }
}
