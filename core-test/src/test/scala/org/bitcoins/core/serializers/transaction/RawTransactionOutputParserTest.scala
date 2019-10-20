package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.Int64
import org.bitcoins.core.protocol.transaction.{
  EmptyTransactionOutput,
  TransactionOutput
}
import org.bitcoins.core.script.bitwise.OP_EQUAL
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant}
import org.bitcoins.core.script.crypto.OP_HASH160
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.testkit.util.BitcoinSUnitTest
import scodec.bits.ByteVector

/**
  * Created by chris on 1/11/16.
  * https://bitcoin.org/en/developer-reference#txout
  */
class RawTransactionOutputParserTest extends BitcoinSUnitTest {

  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawTxOutput =
    "204e00000000000017a914eda8ae08b5c9f973f49543e90a7c292367b3337c87"
  val encode = BitcoinSUtil.encodeHex(_: ByteVector)
  "RawTransactionOutputTest" must "read a serialized tx output" in {

    val txOutput: TransactionOutput =
      RawTransactionOutputParser.read(rawTxOutput)
    txOutput.value must be(Satoshis(Int64(20000)))
    txOutput.scriptPubKey.asm must be(
      Seq(OP_HASH160,
          BytesToPushOntoStack(20),
          ScriptConstant("eda8ae08b5c9f973f49543e90a7c292367b3337c"),
          OP_EQUAL))
  }

  it must "seralialize a transaction output" in {
    val txOutput = RawTransactionOutputParser.read(rawTxOutput)
    encode(RawTransactionOutputParser.write(txOutput)) must be(rawTxOutput)
  }

  it must "serialize a single transaction output not in a sequence" in {
    val txOutput = RawTransactionOutputParser.read(rawTxOutput)
    encode(RawTransactionOutputParser.write(txOutput)) must be(rawTxOutput)
  }

  it must "serialize an older raw transaction output" in {
    //from this question
    //https://bitcoin.stackexchange.com/questions/2859/how-are-transaction-hashes-calculated
    val txOutput =
      "00f2052a01000000434104678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5fac"
    val output = RawTransactionOutputParser.read(txOutput)
    output.value must be(Satoshis(Int64(5000000000L)))
  }

  it must "serialize the empty transaction output correctly" in {
    encode(RawTransactionOutputParser.write(EmptyTransactionOutput)) must be(
      "ffffffffffffffff00")

  }
}
