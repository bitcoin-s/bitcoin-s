package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
  * Created by tom on 6/3/16.
  */
class BlockHeaderTest extends FlatSpec with MustMatchers{
  //genesis block
  //https://en.bitcoin.it/wiki/Genesis_block
  val version = "01000000"
  val prevBlockHash = "0000000000000000000000000000000000000000000000000000000000000000"
  val merkleRoot = "3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A".toLowerCase
  val timeStamp = "29AB5F49".toLowerCase
  val nBits = "FFFF001D".toLowerCase
  val nonce = "1DAC2B7C".toLowerCase
  val hex = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
  "BlockHeader" must "parse a block header" in {
    val blockHeader = RawBlockHeaderSerializer.read(hex)
    blockHeader.version must be (BitcoinSUtil.hexToLong(version))
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
