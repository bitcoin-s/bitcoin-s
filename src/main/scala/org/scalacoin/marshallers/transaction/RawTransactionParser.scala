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
    val versionWithoutPadding = if (txVersionHex.size == 1) "0" + txVersionHex else txVersionHex
    val paddingNeeded = 8 - versionWithoutPadding.size
    val padding = for { i <- 0 until paddingNeeded } yield "0"
    val version = versionWithoutPadding + padding.mkString
    val inputs : String = RawTransactionInputParser.write(tx.inputs)
    val outputs : String = RawTransactionOutputParser.write(tx.outputs)
    
    val lockTimeWithoutPadding : String = flipHalfByte(tx.lockTime.toHexString.reverse)
    val lockTimePaddingNeeded = 8 - lockTimeWithoutPadding.size
    val lockTimePadding = for { i <- 0 until lockTimePaddingNeeded } yield "0"
    val lockTime = lockTimeWithoutPadding + lockTimePadding.mkString

    version + inputs + outputs + lockTime
  }
}


object RawTransactionParser extends RawTransactionParser
