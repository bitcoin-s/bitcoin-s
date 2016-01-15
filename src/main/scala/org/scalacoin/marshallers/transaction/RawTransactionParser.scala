package org.scalacoin.marshallers.transaction

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.transaction.{TransactionImpl, Transaction}
import org.scalacoin.util.{CryptoUtil, ScalacoinUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/14/16.
 * For deserializing and re-serializing a bitcoin transaction
 * https://bitcoin.org/en/developer-reference#raw-transaction-format
 */
trait RawTransactionParser extends RawBitcoinSerializer[Transaction] {

  private lazy val logger = LoggerFactory.getLogger(this.getClass().toString())

  def read(bytes : List[Byte]) = {
    val versionBytes = bytes.take(4)
    val version = Integer.parseInt(ScalacoinUtil.encodeHex(versionBytes.reverse),16)
    val txInputBytes = bytes.slice(4,bytes.size)
    val inputs = RawTransactionInputParser.read(txInputBytes)
    val inputsSize = inputs.map(_.size).sum
    val outputsStartIndex = inputsSize + 5
    val outputsBytes = bytes.slice(outputsStartIndex, bytes.size)
    val outputs = RawTransactionOutputParser.read(outputsBytes)
    val outputsSize = outputs.map(_.size).sum
    val txId = CryptoUtil.doubleSHA256(bytes)
    val lockTimeStartIndex = outputsStartIndex + outputsSize + 1
    val lockTimeBytes = bytes.slice(lockTimeStartIndex, bytes.size)
    val lockTime = Integer.parseInt(ScalacoinUtil.encodeHex(lockTimeBytes.reverse),16)

    TransactionImpl(txId,version,inputs,outputs,lockTime)
  }

  def write(tx : Transaction) : String = {
    //add leading zero if the version byte doesn't require two hex numbers
    val txVersionHex = tx.version.toHexString
    val versionWithoutPadding = addPrecedingZero(txVersionHex)
    val version = addPadding(8,versionWithoutPadding)
    val inputs : String = RawTransactionInputParser.write(tx.inputs)
    val outputs : String = RawTransactionOutputParser.write(tx.outputs)
    val lockTimeWithoutPadding : String = ScalacoinUtil.flipHalfByte(tx.lockTime.toHexString.reverse)
    val lockTime = addPadding(8,lockTimeWithoutPadding)
    version + inputs + outputs + lockTime
  }
}


object RawTransactionParser extends RawTransactionParser
