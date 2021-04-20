package org.bitcoins.crypto

import scodec.bits._

class ECPublicKeyTest extends BitcoinSCryptoTest {

  it must "be able to decompress keys" in {
    val uncompressed =
      ECPublicKey(
        hex"044f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa385b6b1b8ead809ca67454d9683fcf2ba03456d6fe2c4abe2b07f0fbdbb2f1c1")
    val compressed =
      ECPublicKey(
        hex"034f355bdcb7cc0af728ef3cceb9615d90684bb5b2ca5f859ab0f0b704075871aa")

    assert(uncompressed.isFullyValid)
    assert(compressed.isFullyValid)

    assert(compressed.isCompressed)
    assert(!uncompressed.isCompressed)

    assert(compressed.decompressed == uncompressed)
    assert(uncompressed.decompressed == uncompressed)
  }

  it must "generate unique keys" in {
    assert(ECPublicKey() != ECPublicKey())
  }

  it must "decompress keys correctly" in {
    forAll(CryptoGenerators.privateKey) { privKey =>
      val privKeyBytes = ECPrivateKeyBytes(privKey.bytes)
      val pubKey = privKeyBytes.publicKeyBytes

      assert(privKeyBytes.isCompressed)
      assert(pubKey.isCompressed)

      val decompressedPrivKey =
        ECPrivateKeyBytes(privKey.bytes, isCompressed = false)
      val decompressedPubKey = pubKey.decompressed

      assert(decompressedPrivKey.publicKeyBytes == decompressedPubKey)
      assert(pubKey.bytes.tail == decompressedPubKey.bytes.splitAt(33)._1.tail)
    }
  }

  it must "fail if given invalid pubkey" in {
    // 31 bytes
    val badHex =
      "02020202020202020202020202020202020202020202020202020202020202"
    assert(!ECPublicKey.isFullyValid(ByteVector.fromHex(badHex).get))
  }

  it must "be able to compress/decompress public keys" in {
    val privkey = ECPrivateKeyBytes.freshPrivateKey
    assert(CryptoUtil.secKeyVerify(privkey.bytes))
    assert(privkey.isCompressed)

    val notCompressedKey =
      ECPrivateKeyBytes(bytes = privkey.bytes, isCompressed = false)
    val pubkey = notCompressedKey.publicKeyBytes
    assert(CryptoUtil.isValidPubKey(pubkey.bytes))
    assert(!pubkey.isCompressed)

    val compressed = privkey.publicKeyBytes
    assert(CryptoUtil.isValidPubKey(compressed.bytes))
    assert(compressed.isCompressed)

    val converted = pubkey.compressed
    assert(CryptoUtil.isValidPubKey(converted.bytes))
    assert(converted.isCompressed)

    val decompressed = compressed.decompressed
    assert(CryptoUtil.isValidPubKey(decompressed.bytes))
    assert(!decompressed.isCompressed)

    assert(pubkey.bytes != converted.bytes)
    assert(compressed.bytes == converted.bytes)
    assert(compressed.bytes != decompressed.bytes)
    assert(pubkey.bytes == decompressed.bytes)
  }

  it must "not be able to add opposite public keys" in {
    val privkey = ECPrivateKey.freshPrivateKey
    val pubkey1 = privkey.publicKey
    val firstByte: Byte =
      if (pubkey1.bytes.head == 0x02) 0x03
      else if (pubkey1.bytes.head == 0x03) 0x02
      else pubkey1.bytes.head
    val pubkey2 =
      ECPublicKey.fromBytes(ByteVector(firstByte) ++ pubkey1.bytes.tail)

    assertThrows[Exception] {
      val sumKey = CryptoUtil.add(pubkey1, pubkey2)
      if (sumKey == ECPublicKey.infinity) fail()
    }
    assertThrows[Exception] {
      val sumKey = CryptoUtil.add(pubkey1.compressed, pubkey2.compressed)
      if (sumKey == ECPublicKey.infinity) fail()
    }
    assertThrows[Exception] {
      val sumKey = CryptoUtil.add(pubkey1.decompressed, pubkey2.decompressed)
      if (sumKey == ECPublicKey.infinity) fail()
    }
  }

  it must "correctly compress keys" in {
    forAll(CryptoGenerators.privateKey) { privKey =>
      val pubKey = privKey.publicKey
      val pubKeyCompressed = pubKey.compressed
      val pubKeyDecompressed = pubKey.decompressed

      assert(!pubKey.isCompressed)
      assert(pubKeyCompressed.isFullyValid)
      assert(pubKeyDecompressed.isFullyValid)

      assert(pubKeyCompressed == pubKeyDecompressed)
      assert(!pubKeyCompressed.decompressed.isCompressed)
      assert(pubKeyCompressed.compressed.isCompressed)
      assert(pubKeyDecompressed.compressed.isCompressed)
      assert(!pubKeyDecompressed.decompressed.isCompressed)
      assert(pubKeyCompressed == pubKey)
      assert(pubKeyDecompressed == pubKey)
      assert(
        pubKeyCompressed.bytes.tail == pubKeyDecompressed.decompressedBytes
          .splitAt(33)
          ._1
          .tail)
    }
  }

}
