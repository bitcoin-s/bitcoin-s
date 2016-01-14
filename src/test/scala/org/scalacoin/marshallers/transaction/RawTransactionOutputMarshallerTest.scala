package org.scalacoin.marshallers.transaction

import org.scalacoin.currency.{CurrencyUnits, Bitcoins}
import org.scalacoin.protocol.P2SH
import org.scalacoin.protocol.transaction.TransactionOutput
import org.scalacoin.script.bitwise.OP_EQUAL
import org.scalacoin.script.constant.ScriptConstantImpl
import org.scalacoin.script.crypto.OP_HASH160
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/11/16.
 * https://bitcoin.org/en/developer-reference#txout
 */
class RawTransactionOutputParserTest extends FlatSpec with MustMatchers with RawTransactionOutputParser {

  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawTxOutput = "02204e00000000000017a914eda8ae08b5c9f973f49543e90a7c292367b3337c87" +
    "197d2d000000000017a914be2319b9060429692ebeffaa3be38497dc5380c887"
  "RawTransactionOutputTest" must "read a serialized tx output" in {

    val txOutput : Seq[TransactionOutput] = read(rawTxOutput)
    val firstOutput = txOutput.head
    val secondOutput = txOutput(1)
    firstOutput.value must be (CurrencyUnits.toSatoshis(Bitcoins(0.0002)))
    secondOutput.value must be (CurrencyUnits.toSatoshis(Bitcoins(0.02981145)))
    firstOutput.scriptPubKey.asm must be (Seq(OP_HASH160, ScriptConstantImpl("eda8ae08b5c9f973f49543e90a7c292367b3337c"), OP_EQUAL))
    secondOutput.scriptPubKey.asm must be (Seq(OP_HASH160, ScriptConstantImpl("be2319b9060429692ebeffaa3be38497dc5380c8"), OP_EQUAL))
    firstOutput.scriptPubKey.addressType must be (P2SH)
    secondOutput.scriptPubKey.addressType must be (P2SH)
  }

  it must "seralialize a transaction output" in {
    val txOutput = read(rawTxOutput)

    write(txOutput) must be (rawTxOutput)

  }
}
