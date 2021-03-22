package org.bitcoins.crypto

import scodec.bits._

import scala.concurrent.ExecutionContext

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
      val pubKey = privKey.publicKey

      assert(privKey.isCompressed)
      assert(pubKey.isCompressed)

      val decompressedPrivKey =
        ECPrivateKey(privKey.bytes, isCompressed = false)(
          ExecutionContext.global)
      val decompressedPubKey = pubKey.decompressed

      assert(decompressedPrivKey.publicKey == decompressedPubKey)
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
    val privkey = ECPrivateKey.freshPrivateKey
    assert(CryptoUtil.secKeyVerify(privkey.bytes))
    assert(privkey.isCompressed)

    val notCompressedKey =
      ECPrivateKey(bytes = privkey.bytes, isCompressed = false)
    val pubkey = CryptoUtil.toPublicKey(notCompressedKey)
    assert(CryptoUtil.isValidPubKey(pubkey.bytes))
    assert(!pubkey.isCompressed)

    val compressed = privkey.publicKey
    assert(CryptoUtil.isValidPubKey(compressed.bytes))
    assert(compressed.isCompressed)

    val converted =
      CryptoUtil.publicKeyConvert(pubkey, compressed = true)
    assert(CryptoUtil.isValidPubKey(converted.bytes))
    assert(converted.isCompressed)

    val decompressed =
      CryptoUtil.publicKeyConvert(compressed, compressed = false)
    assert(CryptoUtil.isValidPubKey(decompressed.bytes))
    assert(!decompressed.isCompressed)

    assert(pubkey.bytes != converted.bytes)
    assert(compressed.bytes == converted.bytes)
    assert(compressed.bytes != decompressed.bytes)
    assert(pubkey.bytes == decompressed.bytes)
  }

  it must "be able to add infinity points" in {
    val privkey = ECPrivateKey.freshPrivateKey
    val pubkey1 = privkey.publicKey
    val firstByte: Byte =
      if (pubkey1.bytes.head == 0x02) 0x03
      else if (pubkey1.bytes.head == 0x03) 0x02
      else pubkey1.bytes.head
    val pubkey2 =
      ECPublicKey.fromBytes(ByteVector(firstByte) ++ pubkey1.bytes.tail)

    val res1 = CryptoUtil.add(pubkey1, pubkey2)

    assert(res1 == ECPublicKey.infinity)

    val decompressedPubkey1 =
      CryptoUtil.publicKeyConvert(pubkey1, compressed = false)

    val decompressedPubkey2 =
      CryptoUtil.publicKeyConvert(pubkey2, compressed = false)

    val res2 =
      CryptoUtil.add(decompressedPubkey1, decompressedPubkey2)

    assert(res2 == ECPublicKey.infinity)
  }

}
