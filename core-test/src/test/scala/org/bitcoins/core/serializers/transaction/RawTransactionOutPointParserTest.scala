package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

/**
  * Created by chris on 1/11/16.
  */
class RawTransactionOutPointParserTest extends BitcoinSUnitTest {

  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawOutPoint =
    "85d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de100000000"

  val rawOutPointLargeVout =
    "df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd0400134000000"
  val encode = BitcoinSUtil.encodeHex(_: ByteVector)
  "RawTransactionOutPointParser" must "read a raw outpoint into a native scala TransactionOutPoint" in {
    val outPoint = RawTransactionOutPointParser.read(rawOutPoint)
    outPoint.txId.hex must be(
      BitcoinSUtil.flipEndianness(
        "e17d316006850c1764301befcf82c8c84cd1794f3f0d0382b296df2edab0d685"))
    outPoint.vout must be(UInt32.zero)
  }

  it must "parse a large vout for an outpoint" in {
    val outPoint = RawTransactionOutPointParser.read(rawOutPointLargeVout)
    outPoint.vout must be(UInt32(52))
    outPoint.txId.hex must be(
      BitcoinSUtil.flipEndianness(
        "0140d0ed6c9feeb68ea727723a82bbaf0d143fc1d3810265d4dca7ebe6e380df"))
  }
  it must "write a TransactionOutPoint to a serialized format" in {
    val outPoint = RawTransactionOutPointParser.read(rawOutPoint)
    val actualSerialization = RawTransactionOutPointParser.write(outPoint)
    encode(actualSerialization) must be(rawOutPoint)
  }

  it must "write a outpoint that has a large vout" in {
    val outPoint = RawTransactionOutPointParser.read(rawOutPointLargeVout)
    val serializedOutpoint = RawTransactionOutPointParser.write(outPoint)
    encode(serializedOutpoint) must be(rawOutPointLargeVout)
  }

  it must "write this outpoint with vout index 1" in {
    //from txid bdc221db675c06dbee2ae75d33e31cad4e2555efea10c337ff32c8cdf97f8e74
    val rawOutPoint =
      "fc37adbd036fb51b3f4f6f70474270939d6ff8c4ea697639f2b57dd6359e307001000000"

    val outPoint = RawTransactionOutPointParser.read(rawOutPoint)
    outPoint.txId.hex must be(
      BitcoinSUtil.flipEndianness(
        "70309e35d67db5f2397669eac4f86f9d93704247706f4f3f1bb56f03bdad37fc"))
    val serializedOutPoint = RawTransactionOutPointParser.write(outPoint)
    encode(serializedOutPoint) must be(rawOutPoint)
  }

  it must "determine the correct size of a transaction outpoint" in {
    //cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
    val rawOutPoint =
      "85d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de100000000"
    val outPoint = RawTransactionOutPointParser.read(rawOutPoint)
    outPoint.size must be(36)
  }

  it must "parse a outpoint with extremely large vout" in {
    //vout should be 20183580
    val rawOutPoint =
      "4435c4ea162d51135c9b2bbb867a86f25001c246224b60e8ab2307edce7fc28a0ca13f13"
    val outPoint = RawTransactionOutPointParser.read(rawOutPoint)
    outPoint.vout must be(UInt32(322937100))
    outPoint.hex must be(rawOutPoint)
  }

}
