package org.bitcoins.core.serializers

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.transaction.{
  EmptyTransactionOutput,
  TransactionInput,
  TransactionOutput
}
import org.bitcoins.core.serializers.transaction.{
  RawTransactionInputParser,
  RawTransactionOutputParser
}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits.ByteVector

class RawSerializerHelperTest extends BitcoinSUnitTest {

  "RawBitcoinSerializerHelper" must "serialize an empty vector" in {
    val bytes = ByteVector(0.toByte)
    val construct: ByteVector => TransactionInput =
      RawTransactionInputParser.read(_)
    val (inputs, _) =
      RawSerializerHelper.parseCmpctSizeUIntSeq(bytes, construct)

    val serialize = RawTransactionInputParser.write(_)
    val write = RawSerializerHelper.writeCmpctSizeUInt(inputs, serialize)
    write must be(bytes)
  }

  it must "serialize one element in a vector correctly" in {
    val bytes =
      CompactSizeUInt(UInt64.one).bytes ++ EmptyTransactionOutput.bytes
    val constructor: ByteVector => TransactionOutput =
      RawTransactionOutputParser.read(_)

    val (outputs, _) =
      RawSerializerHelper.parseCmpctSizeUIntSeq(bytes, constructor)

    val serialize = RawTransactionOutputParser.write(_)
    val write = RawSerializerHelper.writeCmpctSizeUInt(outputs, serialize)
    write must be(bytes)
  }
}
