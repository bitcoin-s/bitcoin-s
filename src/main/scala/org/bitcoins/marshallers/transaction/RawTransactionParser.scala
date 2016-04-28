package org.bitcoins.marshallers.transaction

import org.bitcoins.marshallers.RawBitcoinSerializer
import org.bitcoins.protocol.transaction.{TransactionImpl, Transaction}
import org.bitcoins.util.{BitcoinSUtil, CryptoUtil}
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
    val version = Integer.parseInt(BitcoinSUtil.encodeHex(versionBytes.reverse),16)
    val txInputBytes = bytes.slice(4,bytes.size)
    val inputs = RawTransactionInputParser.read(txInputBytes)
    val inputsSize = inputs.map(_.size).sum

    val outputsStartIndex = inputsSize + 5
    val outputsBytes = bytes.slice(outputsStartIndex, bytes.size)
    logger.info("Output bytes: " + BitcoinSUtil.encodeHex(outputsBytes))
    val outputs = RawTransactionOutputParser.read(outputsBytes)
    val outputsSize = outputs.map(_.size).sum

    val lockTimeBytes = bytes.slice(bytes.size - 4, bytes.size)
    val lockTime = Integer.parseInt(BitcoinSUtil.encodeHex(lockTimeBytes.reverse),16)

    TransactionImpl(version,inputs,outputs,lockTime)
  }

  def write(tx : Transaction) : String = {
    //add leading zero if the version byte doesn't require two hex numbers
    val txVersionHex = tx.version.toHexString
    val versionWithoutPadding = addPrecedingZero(txVersionHex)
    val version = addPadding(8,versionWithoutPadding)
    val inputs : String = RawTransactionInputParser.write(tx.inputs)
    val outputs : String = RawTransactionOutputParser.write(tx.outputs)
    val lockTimeWithoutPadding : String = BitcoinSUtil.flipHalfByte(tx.lockTime.toHexString.reverse)
    val lockTime = addPadding(8,lockTimeWithoutPadding)
    version + inputs + outputs + lockTime
  }
}


object RawTransactionParser extends RawTransactionParser
