package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, CryptoUtil}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 1/14/16.
 * For deserializing and re-serializing a bitcoin transaction
 * https://bitcoin.org/en/developer-reference#raw-transaction-format
 */
trait RawTransactionParser extends RawBitcoinSerializer[Transaction] with BitcoinSLogger {

  def read(bytes : List[Byte]) = {
    val versionBytes = bytes.take(4)
    logger.info("Version bytes: " + BitcoinSUtil.encodeHex(versionBytes))
    val version = UInt32(versionBytes.reverse).underlying
    logger.info("UInt32 version: " + version)
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
    logger.info("Tx version: " + tx.version)
    //add leading zero if the version byte doesn't require two hex numbers
    val txVersionHex = tx.version.toHexString
    logger.info("txVersionHex: " + txVersionHex)
    val versionWithoutPadding = BitcoinSUtil.flipEndianess(addPrecedingZero(txVersionHex))
    logger.info("Version wthout padding: " + versionWithoutPadding)
    val version = addPadding(8,versionWithoutPadding)
    logger.info("Version: " + version)
    val inputs : String = RawTransactionInputParser.write(tx.inputs)
    val outputs : String = RawTransactionOutputParser.write(tx.outputs)
    val lockTimeWithoutPadding : String = tx.lockTime.toHexString
    val lockTime = BitcoinSUtil.flipEndianess(addPadding(8,lockTimeWithoutPadding))
    version + inputs + outputs + lockTime
  }
}


object RawTransactionParser extends RawTransactionParser
