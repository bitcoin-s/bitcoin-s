package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.BaseTransaction
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil}

/**
 * Created by chris on 1/14/16.
 * For deserializing and re-serializing a bitcoin transaction
 * https://bitcoin.org/en/developer-reference#raw-transaction-format
 */
trait RawBaseTransactionParser extends RawBitcoinSerializer[BaseTransaction] with BitcoinSLogger {

  def read(bytes : List[Byte]): BaseTransaction = {
    val versionBytes = bytes.take(4)
    val version = UInt32(versionBytes.reverse)
    val txInputBytes = bytes.slice(4,bytes.size)
    val inputs = RawTransactionInputParser.read(txInputBytes)
    val inputsSize = inputs.map(_.size).sum

    val outputsStartIndex = inputsSize + 5
    val outputsBytes = bytes.slice(outputsStartIndex, bytes.size)
    logger.debug("outputBytes: " + BitcoinSUtil.encodeHex(outputsBytes))
    val outputs = RawTransactionOutputParser.read(outputsBytes)
    val lockTimeStartIndex = outputsStartIndex + outputs.map(_.size).sum + 1

    val lockTimeBytes = bytes.slice(lockTimeStartIndex, lockTimeStartIndex + 4)

    val lockTime = UInt32(lockTimeBytes.reverse)

    BaseTransaction(version,inputs,outputs,lockTime)
  }

  def write(tx : BaseTransaction) : String = {
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


object RawBaseTransactionParser extends RawBaseTransactionParser
