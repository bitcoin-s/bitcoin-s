package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  TransactionInput,
  TransactionOutput
}
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import scodec.bits.ByteVector

/**
  * Created by chris on 1/14/16.
  * For deserializing and re-serializing a bitcoin transaction
  * https://bitcoin.org/en/developer-reference#raw-transaction-format
  */
sealed abstract class RawBaseTransactionParser
    extends RawBitcoinSerializer[BaseTransaction] {

  val helper = RawSerializerHelper

  def read(bytes: ByteVector): BaseTransaction = {
    val versionBytes = bytes.take(4)
    val version = Int32(versionBytes.reverse)
    val txInputBytes = bytes.slice(4, bytes.size)
    val (inputs, outputBytes) = helper.parseCmpctSizeUIntSeq(
      txInputBytes,
      RawTransactionInputParser.read(_))
    val (outputs, lockTimeBytes) = helper.parseCmpctSizeUIntSeq(
      outputBytes,
      RawTransactionOutputParser.read(_))
    val lockTime = UInt32(lockTimeBytes.take(4).reverse)
    BaseTransaction(version, inputs, outputs, lockTime)
  }

  def write(tx: BaseTransaction): ByteVector = {
    val version = tx.version.bytes.reverse
    val inputs: ByteVector = helper.writeCmpctSizeUInt[TransactionInput](
      tx.inputs,
      RawTransactionInputParser.write(_))
    val outputs: ByteVector = helper.writeCmpctSizeUInt[TransactionOutput](
      tx.outputs,
      RawTransactionOutputParser.write(_))
    val lockTime = tx.lockTime.bytes.reverse
    version ++ inputs ++ outputs ++ lockTime
  }
}

object RawBaseTransactionParser extends RawBaseTransactionParser
