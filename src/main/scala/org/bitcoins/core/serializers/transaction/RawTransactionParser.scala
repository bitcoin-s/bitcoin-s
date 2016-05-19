package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.protocol.transaction.{Transaction}
import org.bitcoins.core.util.{BitcoinSUtil, CryptoUtil}
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

    val lockTimeBytes = bytes.slice(bytes.size - 4, bytes.size)
    val lockTime = java.lang.Long.parseLong(BitcoinSUtil.encodeHex(lockTimeBytes.reverse),16)

    Transaction(version,inputs,outputs,lockTime)
  }

  def write(tx : Transaction) : String = {
    //add leading zero if the version byte doesn't require two hex numbers
    val txVersionHex = tx.version.toHexString
    val versionWithoutPadding = addPrecedingZero(txVersionHex)
    val version = addPadding(8,versionWithoutPadding)
    val inputs : String = RawTransactionInputParser.write(tx.inputs)
    val outputs : String = RawTransactionOutputParser.write(tx.outputs)
    val lockTimeWithoutPadding : String = tx.lockTime.toHexString
    val lockTime = BitcoinSUtil.flipEndianess(addPadding(8,lockTimeWithoutPadding))
    version + inputs + outputs + lockTime
  }
}


object RawTransactionParser extends RawTransactionParser
