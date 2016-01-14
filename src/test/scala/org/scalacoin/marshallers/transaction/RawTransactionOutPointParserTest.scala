package org.scalacoin.marshallers.transaction

import org.scalacoin.protocol.transaction.TransactionOutPointImpl
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/11/16.
 */
class RawTransactionOutPointParserTest extends FlatSpec with MustMatchers  {
  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawOutPoint = "85d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de100000000"
  "RawTransactionOutPointMarshaller" must "read a raw outpoint into a native scala TransactionOutPoint" in {
    val outPoint = RawTransactionOutPointParser.read(rawOutPoint)
    outPoint.txId must be ("e17d316006850c1764301befcf82c8c84cd1794f3f0d0382b296df2edab0d685")
    outPoint.vout must be (0)
  }

  it must "write a TransactionOutPoint to a serialized format" in {
    val outPoint = RawTransactionOutPointParser.read(rawOutPoint)
    val actualSerialization = RawTransactionOutPointParser.write(outPoint)
    actualSerialization must be (rawOutPoint)
  }


}
