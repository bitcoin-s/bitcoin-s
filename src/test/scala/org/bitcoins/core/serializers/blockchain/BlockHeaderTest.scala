package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by tom on 6/3/16.
  */
class BlockHeaderTest extends FlatSpec with MustMatchers{
  val version = "02000000"
  val prevBlockHash = "b6ff0b1b1680a2862a30ca44d346d9e8910d334beb48ca0c0000000000000000"
  val merkleRoot = "9d10aa52ee949386ca9385695f04ede270dda20810decd12bc9b048aaab31471"
  val timeStamp = "24d95a54"
  val nBits = "30c31b18"
  val nonce = "fe9f0864"
  val hex = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
  "BlockHeader" must "parse a block header" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    blockHeader.version must be (2)
    blockHeader.previousBlockHash must be (DoubleSha256Digest(prevBlockHash))
    blockHeader.merkleRootHash must be (DoubleSha256Digest(merkleRoot))
    blockHeader.time must be (BitcoinSUtil.hexToLong(timeStamp))
    blockHeader.nBits must be (BitcoinSUtil.hexToLong(nBits))
    blockHeader.nonce must be (BitcoinSUtil.hexToLong(nonce))
  }

  it must "write a block header" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    RawBlockHeaderSerializer.write(blockHeader) must be (hex)
  }
}
