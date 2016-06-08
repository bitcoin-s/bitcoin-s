package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.serializers.blockchain
import org.bitcoins.core.serializers.transaction.RawTransactionParser
import org.bitcoins.core.util.{BitcoinSLogger, CryptoUtil, BitcoinSUtil}
import org.scalatest.{MustMatchers, FlatSpec}

/**
  * Created by tom on 6/3/16.
  */
class RawBlockSerializerTest extends FlatSpec with MustMatchers with BitcoinSLogger {
  //genesis block
  //https://en.bitcoin.it/wiki/Genesis_block
  //https://webbtc.com/block/000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
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

  "RawBlockSerializer" must "parse genesis block" in {
    val block = RawBlockSerializer.read(hex)
    block.txCount.num must be (txSeq.size)
    block.txCount must be (uInt)
    block.blockHeader must be (RawBlockHeaderSerializer.read(header))
    block.transactions must be (txSeq)
    block.blockHeader.hash.hex must be ("000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f")
    block.hex must be (hex)
  }

  it must "write genesis block" in {
    val block = RawBlockSerializer.read(hex)
    RawBlockSerializer.write(block) must be(hex)
  }

  it must "parse/write a block other than genesis" in {
    //https://test-insight.bitpay.com/block/00000000009fdf81e6efa251bc1902aedc07dc499cbe17db6e33db61eb64a7c5
    val version = "00000020"
    val merkleRoot = "8dfb7407a20ac5c6bd890e56a37d41ceafae3d13e32ff8a5e63b72bc60214e24" // flipped
    val prevBlockHash = "06cd0fd35e1a102b93004d738561b56be94da29692d3b1d0c3dcf20000000000" //flipped
    val timeStamp = "0ae75557" //1465247498
    val nBits = "FFFF001D".toLowerCase
    val nonce = "43e3fe9e"
    val header = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
    val rawTx1 = "0100000001000000000000000000000000000000000000000000000000000000000" +
      "0000000ffffffff04031d410dffffffff04f8f3b1110000000017a914349ef962198fcc875f45" +
      "e786598272ecace9818d87286bee000000000017a914349ef962198fcc875f45e786598272ecac" +
      "e9818d870000000000000000226a20000000000000000000000000000000000000000000000000" +
      "0000ffff0000000000000000000000000a6a08ef5f706c2f00000000000000"
    val tx1 = Transaction(rawTx1)
    val txSeq = List(tx1)
    val uInt = CompactSizeUInt(1, 1)
    val hex = header + uInt.hex + rawTx1
    //matches real hex from
    //https://test.webbtc.com/block/00000000009fdf81e6efa251bc1902aedc07dc499cbe17db6e33db61eb64a7c5.hex
    val block = RawBlockSerializer.read(hex)
    block.blockHeader.hex must be (header)
    block.transactions must be (txSeq)
    block.blockHeader.hash.hex must be ("00000000009fdf81e6efa251bc1902aedc07dc499cbe17db6e33db61eb64a7c5")
    block.hex must be (hex)
    block.txCount must be (uInt)
    block.txCount.num must be (1)
  }
}