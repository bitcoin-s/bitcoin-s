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
    val version = UInt32(versionBytes.reverse)
    val txInputBytes = bytes.slice(4,bytes.size)
    val inputs = RawTransactionInputParser.read(txInputBytes)
    val inputsSize = inputs.map(_.size).sum

    val outputsStartIndex = inputsSize + 5
    val outputsBytes = bytes.slice(outputsStartIndex, bytes.size)
    val outputs = RawTransactionOutputParser.read(outputsBytes)
    val lockTimeStartIndex = outputsStartIndex + outputs.map(_.size).sum + 1

    val lockTimeBytes = bytes.slice(lockTimeStartIndex, lockTimeStartIndex + 4)

    val lockTime = UInt32(lockTimeBytes.reverse)

    Transaction(version,inputs,outputs,lockTime)
  }

  def write(tx : Transaction) : String = {
    //add leading zero if the version byte doesn't r.hexre two hex numbers
    val txVersionHex = tx.version.hex
    val version = BitcoinSUtil.flipEndianness(txVersionHex)
    val inputs : String = RawTransactionInputParser.write(tx.inputs)
    val outputs : String = RawTransactionOutputParser.write(tx.outputs)
    val lockTimeWithoutPadding : String = tx.lockTime.hex
    val lockTime = addPadding(8,BitcoinSUtil.flipEndianness(lockTimeWithoutPadding))
    version + inputs + outputs + lockTime
  }
}


object RawTransactionParser extends RawTransactionParser
