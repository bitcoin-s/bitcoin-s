package org.scalacoin.marshallers.transaction

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.transaction.TransactionOutput
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/11/16.
 * https://bitcoin.org/en/developer-reference#txout
 */
class RawTransactionOutputMarshallerTest extends FlatSpec with MustMatchers {

  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawTxOutput = "02204e00000000000017a914eda8ae08b5c9f973f49543e90a7c292367b3337c87197d2d000000000017a914be2319b9060429692ebeffaa3be38497dc5380c887"
  "RawTransactionOutputMarshaller" must "read a serialized tx output" in {

    val _ : Seq[TransactionOutput] = ???
  }
}
