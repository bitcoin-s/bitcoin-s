package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.serializers.blockchain
import org.bitcoins.core.serializers.transaction.RawTransactionParser
import org.bitcoins.core.util.BitcoinSUtil
import org.scalatest.{MustMatchers, FlatSpec}

/**
  * Created by tom on 6/3/16.
  */
class BlockSerializerTest extends FlatSpec with MustMatchers {
  //genesis block
  //https://en.bitcoin.it/wiki/Genesis_block
  val version = "01000000"
  val prevBlockHash = "0000000000000000000000000000000000000000000000000000000000000000"
  val merkleRoot = "3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A".toLowerCase
  val timeStamp = "29AB5F49".toLowerCase
  val nBits = "FFFF001D".toLowerCase
  val nonce = "1DAC2B7C".toLowerCase
  val header = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
  val rawTx1 = "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4" +
    "d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e2062726" +
    "96e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678" +
    "afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c3" +
    "84df7ba0b8d578a4c702b6bf11d5fac00000000"
  val tx1 = Transaction(rawTx1)
  val txSeq = List(tx1)
  val uInt = CompactSizeUInt(1, 1)
  val hex = header + uInt.hex + rawTx1

  "RawBlockSerializer" must "parse a block" in {
    val block = RawBlockSerializer.read(hex)
    block.txCount.num must be (txSeq.size)
    block.blockHeader must be (RawBlockHeaderSerializer.read(header))
    block.transactions must be (List(RawTransactionParser.read(rawTx1)))
  }

  it must "write a block" in {
    val block = RawBlockSerializer.read(hex)
    RawBlockSerializer.write(block) must be(hex)
  }
}