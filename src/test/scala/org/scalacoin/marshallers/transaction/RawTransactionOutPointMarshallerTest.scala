package org.scalacoin.marshallers.transaction

import org.scalacoin.protocol.transaction.TransactionOutPointImpl
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/11/16.
 */
class RawTransactionOutPointMarshallerTest extends FlatSpec with MustMatchers  {
  val rawOutPoint = "c77ada2138ae4fa8d6c4de0398a14f3f00000000"
  val outPoint = TransactionOutPointImpl("c77ada2138ae4fa8d6c4de0398a14f3f",0)
  "RawTransactionOutPointMarshaller" must "read a raw outpoint into a native scala TransactionOutPoint" in {
    val outPoint = RawTransactionOutPointMarshaller.read(rawOutPoint)
    outPoint.txId must be ("c77ada2138ae4fa8d6c4de0398a14f3f")
    outPoint.vout must be (0)
  }

  it must "write a TransactionOutPoint to a serialized format" in {

    val actualSerialization = RawTransactionOutPointMarshaller.write(outPoint)
    actualSerialization must be (rawOutPoint)
  }

  it must "serialize a transaction outpoint then unserialize it again" in {
    val serialization = RawTransactionOutPointMarshaller.write(outPoint)
    val actualOutPoint = RawTransactionOutPointMarshaller.read(serialization)

    actualOutPoint must be (outPoint)
  }

}
