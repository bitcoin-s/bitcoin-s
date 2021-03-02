package org.bitcoins.core.serializers.blockchain

import org.bitcoins.core.number.{Int32, UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.BytesUtil
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

/** Created by tom on 6/3/16.
  */
class RawBlockSerializerTest extends BitcoinSUnitTest {
  //genesis block
  //https://en.bitcoin.it/wiki/Genesis_block
  //https://webbtc.com/block/000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
  val version = "01000000"

  val prevBlockHash =
    "0000000000000000000000000000000000000000000000000000000000000000"

  val merkleRoot =
    "3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A".toLowerCase
  val timeStamp = "29AB5F49".toLowerCase
  val nBits = "FFFF001D".toLowerCase
  val nonce = "1DAC2B7C".toLowerCase
  val hash = "000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"
  val header = version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce

  val rawTx1 =
    "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff4" +
      "d04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e2062726" +
      "96e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678" +
      "afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c3" +
      "84df7ba0b8d578a4c702b6bf11d5fac00000000"
  val tx1 = Transaction(rawTx1)
  val txSeq = List(tx1)
  val uInt = CompactSizeUInt(UInt64.one, 1)
  val hex = header + uInt.hex + rawTx1
  val encode = BytesUtil.encodeHex(_: ByteVector)
  "RawBlockSerializer" must "parse genesis block" in {
    val block = RawBlockSerializer.read(hex)
    block.txCount.num must be(UInt64(txSeq.size))
    block.txCount must be(uInt)
    block.blockHeader must be(RawBlockHeaderSerializer.read(header))
    block.transactions must be(txSeq)
    block.blockHeader.hash.hex must be(
      "6fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000")
    block.hex must be(hex)
  }

  it must "write genesis block" in {
    val block = RawBlockSerializer.read(hex)
    encode(RawBlockSerializer.write(block)) must be(hex)
  }

  it must "parse/write a block other than genesis" in {
    //https://test-insight.bitpay.com/block/00000000009fdf81e6efa251bc1902aedc07dc499cbe17db6e33db61eb64a7c5
    val version = "00000020"
    val merkleRoot =
      "8dfb7407a20ac5c6bd890e56a37d41ceafae3d13e32ff8a5e63b72bc60214e24"
    val prevBlockHash =
      "06cd0fd35e1a102b93004d738561b56be94da29692d3b1d0c3dcf20000000000"
    val timeStamp = "0ae75557" //1465247498
    val nBits = "FFFF001D".toLowerCase
    val nonce = "43e3fe9e"
    val header =
      version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
    val rawTx1 =
      "0100000001000000000000000000000000000000000000000000000000000000000" +
        "0000000ffffffff04031d410dffffffff04f8f3b1110000000017a914349ef962198fcc875f45" +
        "e786598272ecace9818d87286bee000000000017a914349ef962198fcc875f45e786598272ecac" +
        "e9818d870000000000000000226a20000000000000000000000000000000000000000000000000" +
        "0000ffff0000000000000000000000000a6a08ef5f706c2f00000000000000"
    val tx1 = Transaction(rawTx1)
    val txSeq = List(tx1)
    val uInt = CompactSizeUInt(UInt64.one, 1)
    val hex = header + uInt.hex + rawTx1
    //matches real hex from
    //https://test.webbtc.com/block/00000000009fdf81e6efa251bc1902aedc07dc499cbe17db6e33db61eb64a7c5.hex
    val block = RawBlockSerializer.read(hex)
    block.blockHeader.hex must be(header)
    block.transactions must be(txSeq)
    block.blockHeader.hash.hex must be(
      "c5a764eb61db336edb17be9c49dc07dcae0219bc51a2efe681df9f0000000000")
    block.hex must be(hex)
    block.txCount must be(uInt)
    block.txCount.num must be(UInt64(1))

    encode(RawBlockSerializer.write(block)) must be(hex)
  }

  it must "parse block with more than 1 transaction" in {
    //https://test.webbtc.com/block/000000000000189efaecf3ed9108b9de5320671c66a161a038f87b3e11493d5f.json
    val version = "00000020"
    val prevBlockHash =
      "cde33b7c310995d9e84777f3f409aa9f2cefe42ff6723d4a3174d80000000000"
    val merkleRoot =
      "cf836ca863a6ee66d49e5769642de08898b622774e4d2617b94dea247c60a685"
    val timeStamp = "90895757" //in hex. 1465354640 as long
    val nBits = "6271191a"
    val nonce = "8f548695"
    val header =
      version + prevBlockHash + merkleRoot + timeStamp + nBits + nonce
    val rawTx1 =
      "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff1f0" +
        "343430d2f48616f4254432f48616f427463506f6f6c2f0504ecfde9ff0400ffffffff01757fa012000000001976a9149" +
        "f9a1576ec07a376fa1955bd063674cc341154d188ac00000000"
    val rawTx2 = "01000000019d531b0d5ec657e4596ef4547ff267f53fde1baf81ebb57c815118c3b56523c200000000fdfd" +
      "000047304402203ef7cf2baf06c0eaaf7f7dad16cfd6e36a6b2f7d302ab5a1718eaa1e1fa791a102201dab8db8f94e39" +
      "906a24985fc8fdede92a0ed837ca53d8c82665df5893292dbc01483045022100e06e62130f7063623ef3ea89c2a3053bb" +
      "d913725ec335c28204fdbb2ea184c5e022029976757c486e01d3c8d11c829b1a1582896a25f46389929aeb12406a6df56" +
      "6e014c69522102f831adc4a8c0c3e82a22ed0d728b5a5a21441d781377d42a6e5940dad7b3ff4e2102a31dc1bf20eab97" +
      "7524c96a76cd80c6c8615f4dd035c06a5dba1ccebda27fade2102578d2b1dca3b30f528be2ab5c7477f7972a9ecdf3951" +
      "1a23fb2d05298d7411e753aeffffffff0200c2eb0b000000001976a9147ed48eec58430bd1f9c1d75120797d9a2ca4285" +
      "a88ac0fb309680000000017a914e33cea5fe24462736f6ce6b7c6d501ced23b23228700000000"
    val tx1 = Transaction(rawTx1)
    val tx2 = Transaction(rawTx2)
    val txSeq = List(tx1, tx2)
    val uInt = CompactSizeUInt(UInt64(2), 1)
    val hex = header + uInt.hex + rawTx1 + rawTx2

    val block = RawBlockSerializer.read(hex)
    block.blockHeader.hash.hex must be(
      "5f3d49113e7bf838a061a1661c672053deb90891edf3ecfa9e18000000000000")
    block.blockHeader.version must be(Int32(536870912))
    block.blockHeader.previousBlockHash must be(
      DoubleSha256Digest(prevBlockHash))
    block.blockHeader.merkleRootHash must be(DoubleSha256Digest(merkleRoot))
    block.blockHeader.time must be(UInt32(1465354640))
    block.blockHeader.nBits must be(UInt32(437875042))
    block.blockHeader must be(RawBlockHeaderSerializer.read(header))
    block.txCount.num must be(UInt64(txSeq.size))
    block.hex must be(hex)
  }
}
