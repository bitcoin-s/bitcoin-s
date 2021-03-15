package org.bitcoins.crypto

import scodec.bits.ByteVector

class KeysTest extends BitcoinSCryptoTest {

  it must "generate keys" in {
    val privkey = ECPrivateKey.freshPrivateKey
    assert(privkey != null)
    assert(BCryptoCryptoRuntime.secKeyVerify(privkey.bytes))

    val pubkey = privkey.publicKey
    assert(pubkey != null)
    assert(BCryptoCryptoRuntime.isValidPubKey(pubkey.bytes))

    assert(!BCryptoCryptoRuntime.secKeyVerify(pubkey.bytes))
    assert(!BCryptoCryptoRuntime.isValidPubKey(privkey.bytes))
  }

  it must "be able to compress/decompress public keys" in {
    val privkey = ECPrivateKey.freshPrivateKey
    assert(BCryptoCryptoRuntime.secKeyVerify(privkey.bytes))
    assert(privkey.isCompressed)

    val pubkey = BCryptoCryptoRuntime.toPublicKey(privkey, isCompressed = false)
    assert(BCryptoCryptoRuntime.isValidPubKey(pubkey.bytes))
    assert(!pubkey.isCompressed)

    val compressed = privkey.publicKey
    assert(BCryptoCryptoRuntime.isValidPubKey(compressed.bytes))
    assert(compressed.isCompressed)

    val converted = ECPublicKey.fromBytes(
      BCryptoCryptoRuntime.publicKeyConvert(pubkey.bytes, compressed = true))
    assert(BCryptoCryptoRuntime.isValidPubKey(converted.bytes))
    assert(converted.isCompressed)

    val decompressed = ECPublicKey.fromBytes(
      BCryptoCryptoRuntime.publicKeyConvert(compressed.bytes,
                                            compressed = false))
    assert(BCryptoCryptoRuntime.isValidPubKey(decompressed.bytes))
    assert(!decompressed.isCompressed)

    assert(pubkey.bytes != converted.bytes)
    assert(compressed.bytes == converted.bytes)
    assert(compressed.bytes != decompressed.bytes)
    assert(pubkey.bytes == decompressed.bytes)
  }

  private val inf = ECPublicKey.fromHex("00")

  it must "be able to add infinity points" in {
    val privkey = ECPrivateKey.freshPrivateKey
    val pubkey1 = privkey.publicKey
    val firstByte: Byte =
      if (pubkey1.bytes.head == 0x02) 0x03
      else if (pubkey1.bytes.head == 0x03) 0x02
      else pubkey1.bytes.head
    val pubkey2 =
      ECPublicKey.fromBytes(ByteVector(firstByte) ++ pubkey1.bytes.tail)

    val res1 = BCryptoCryptoRuntime.add(pubkey1, pubkey2)

    assert(res1 == inf)

    val decompressedPubkey1 = ECPublicKey.fromBytes(
      BCryptoCryptoRuntime.publicKeyConvert(pubkey1.bytes, compressed = false))

    val decompressedPubkey2 = ECPublicKey.fromBytes(
      BCryptoCryptoRuntime.publicKeyConvert(pubkey2.bytes, compressed = false))

    val res2 =
      BCryptoCryptoRuntime.add(decompressedPubkey1, decompressedPubkey2)

    assert(res2 == inf)
  }

}
