package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by tom on 6/3/16.
  */
class RawBlockHeaderSerializerTest extends FlatSpec with MustMatchers{
  //genesis block
  //https://en.bitcoin.it/wiki/Genesis_block
  //https://insight.bitpay.com/block/000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
  val version = "01000000"
  val prevBlockHash = "0000000000000000000000000000000000000000000000000000000000000000"
  //val merkleRoot = "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"
  val merkleRoot = "3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A".toLowerCase
  val timeStamp = "29AB5F49".toLowerCase
  val nBits = "FFFF001D".toLowerCase
  val nonce = "1DAC2B7C".toLowerCase
  val hash = "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"

  val hex = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
  "BlockHeader" must "parse genesis block header" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    blockHeader.version must be (java.lang.Long.parseLong(BitcoinSUtil.flipEndianess(version), 16))
    blockHeader.previousBlockHash must be (DoubleSha256Digest(prevBlockHash))
    blockHeader.merkleRootHash must be (DoubleSha256Digest(merkleRoot))
    blockHeader.time must be (java.lang.Long.parseLong(BitcoinSUtil.flipEndianess(timeStamp), 16))
    blockHeader.nBits must be (java.lang.Long.parseLong(BitcoinSUtil.flipEndianess(nBits), 16))
    blockHeader.nonce must be (java.lang.Long.parseLong(BitcoinSUtil.flipEndianess(nonce), 16))
  }

  it must "properly hash genesis block header to return genesis block hash" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    blockHeader.hash must be (DoubleSha256Digest(hash))
  }

  it must "write a block header" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    RawBlockHeaderSerializer.write(blockHeader) must be (hex)
  }

  it must "parse different block header with known values from dev reference" in {
    //from bitcoin developer reference
    //https://bitcoin.org/en/developer-reference#block-headers
    //https://insight.bitpay.com/block/000000000000000009a11b3972c8e532fe964de937c9e0096b43814e67af3728
    val version2 = "02000000"
    val prevBlockHash2 = "b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c0000000000000000"
    val merkleRoot2 = "9d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab31471"
    val timeStamp2 = "24d95a54"
    val nBits2 = "30c31b18"
    val nonce2 = "fe9f0864"
    val hex2 = version2 + prevBlockHash2 + merkleRoot2 + timeStamp2 + nBits2 + nonce2
    val hash = "000000000000000009a11b3972c8e532fe964de937c9e0096b43814e67af3728"
    val blockHeader = RawBlockHeaderSerializer.read(hex2)
    blockHeader.version must be (2)
    blockHeader.previousBlockHash must be (DoubleSha256Digest(prevBlockHash2))
    blockHeader.merkleRootHash must be (DoubleSha256Digest(merkleRoot2))
    blockHeader.time must be (1415239972)
    blockHeader.nonce must be (1678286846)
    blockHeader.hash must be (DoubleSha256Digest(hash))
  }

  it must "parse/write testnet block header" in {
    //https://test-insight.bitpay.com/block/00000000009fdf81e6efa251bc1902aedc07dc499cbe17db6e33db61eb64a7c5
    val version = "00000020"
    val prevBlockHash = BitcoinSUtil.flipEndianess("0000000000f2dcc3d0b1d39296a24de96bb56185734d00932b101a5ed30fcd06")
    val merkleRoot = BitcoinSUtil.flipEndianess("244e2160bc723be6a5f82fe3133daeafce417da3560e89bdc6c50aa20774fb8d")
    val timeStamp = "0ae75557" //1465247498
    val nBits = "FFFF001D".toLowerCase
    val nonce = "43e3fe9e"
    val hex = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
    val hash = "00000000009fdf81e6efa251bc1902aedc07dc499cbe17db6e33db61eb64a7c5"
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    blockHeader.version must be (536870912)
    blockHeader.previousBlockHash must be (DoubleSha256Digest("06cd0fd35e1a102b93004d738561b56be94da29692d3b1d0c3dcf20000000000"))
    blockHeader.merkleRootHash must be (DoubleSha256Digest("8dfb7407a20ac5c6bd890e56a37d41ceafae3d13e32ff8a5e63b72bc60214e24"))
    blockHeader.time must be (1465247498)
    blockHeader.nBits must be (486604799)
    blockHeader.nonce must be (2667504451L)
    blockHeader.hash must be (DoubleSha256Digest(hash))
    RawBlockHeaderSerializer.write(blockHeader) must be (hex)
  }
}
