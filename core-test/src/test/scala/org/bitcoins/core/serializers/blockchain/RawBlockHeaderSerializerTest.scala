package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

/** Values transmitted in the network are big-endian.
  * Created by tom on 6/3/16.
  */
class RawBlockHeaderSerializerTest extends BitcoinSUnitTest {
  //genesis block
  //https://en.bitcoin.it/wiki/Genesis_block
  //https://insight.bitpay.com/block/000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
  val version = "01000000"

  val prevBlockHash =
    "0000000000000000000000000000000000000000000000000000000000000000"

  val merkleRoot =
    "3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A".toLowerCase
  val timeStamp = "29AB5F49".toLowerCase
  val nBits = "FFFF001D".toLowerCase
  val nonce = "1DAC2B7C".toLowerCase
  val hash = "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000"
  val encode = BytesUtil.encodeHex(_: ByteVector)
  val hex = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
  "BlockHeader" must "parse genesis block header" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    blockHeader.version must be(Int32(BytesUtil.flipEndianness(version)))
    blockHeader.previousBlockHash must be(DoubleSha256Digest(prevBlockHash))
    blockHeader.merkleRootHash must be(DoubleSha256Digest(merkleRoot))
    blockHeader.time must be(UInt32(BytesUtil.flipEndianness(timeStamp)))
    blockHeader.nBits must be(UInt32(BytesUtil.flipEndianness(nBits)))
    blockHeader.nonce must be(UInt32(BytesUtil.flipEndianness(nonce)))
  }

  it must "properly hash genesis block header to return genesis block hash" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    blockHeader.hash must be(DoubleSha256Digest(hash))
  }

  it must "write genesis block header" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    encode(RawBlockHeaderSerializer.write(blockHeader)) must be(hex)
  }

  it must "parse different block header with known values from dev reference" in {
    //from bitcoin developer reference
    //https://bitcoin.org/en/developer-reference#block-headers
    //https://insight.bitpay.com/block/000000000000000009a11b3972c8e532fe964de937c9e0096b43814e67af3728
    val version2 = "02000000"
    val prevBlockHash2 =
      "b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c0000000000000000"
    val merkleRoot2 =
      "9d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab31471"
    val timeStamp2 = "24d95a54"
    val nBits2 = "30c31b18"
    val nonce2 = "fe9f0864"
    val hex2 =
      version2 + prevBlockHash2 + merkleRoot2 + timeStamp2 + nBits2 + nonce2
    val hash =
      "2837af674e81436b09e0c937e94d96fe32e5c872391ba1090000000000000000"
    val blockHeader = RawBlockHeaderSerializer.read(hex2)
    blockHeader.version must be(Int32(2))
    blockHeader.previousBlockHash must be(DoubleSha256Digest(prevBlockHash2))
    blockHeader.merkleRootHash must be(DoubleSha256Digest(merkleRoot2))
    blockHeader.time must be(UInt32(1415239972))
    blockHeader.nonce must be(UInt32(1678286846))
    blockHeader.hash must be(DoubleSha256Digest(hash))
  }

  it must "parse/write testnet block header" in {
    //https://test-insight.bitpay.com/block/00000000009fdf81e6efa251bc1902aedc07dc499cbe17db6e33db61eb64a7c5
    val version = "00000020"
    val prevBlockHash =
      "06cd0fd35e1a102b93004d738561b56be94da29692d3b1d0c3dcf20000000000"
    val merkleRoot =
      "8dfb7407a20ac5c6bd890e56a37d41ceafae3d13e32ff8a5e63b72bc60214e24"
    val timeStamp = "0ae75557" //1465247498
    val nBits = "FFFF001D".toLowerCase
    val nonce = "43e3fe9e"
    val hex = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
    val hash =
      "c5a764eb61db336edb17be9c49dc07dcae0219bc51a2efe681df9f0000000000"
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    blockHeader.version must be(Int32(536870912))
    blockHeader.previousBlockHash must be(DoubleSha256Digest(prevBlockHash))
    blockHeader.merkleRootHash must be(DoubleSha256Digest(merkleRoot))
    blockHeader.time must be(UInt32(1465247498))
    blockHeader.nBits must be(UInt32(486604799))
    blockHeader.nonce must be(UInt32(2667504451L))
    blockHeader.hash must be(DoubleSha256Digest(hash))
    encode(RawBlockHeaderSerializer.write(blockHeader)) must be(hex)
  }
}
