package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{TransactionWitness, WitnessTransaction}
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSUtil

/**
  * Created by chris on 11/21/16.
  */
trait RawWitnessTransactionParser extends RawBitcoinSerializer[WitnessTransaction] {

  def read(bytes: List[Byte]): WitnessTransaction = {
    val versionBytes = bytes.take(4)
    val version = UInt32(versionBytes.reverse)
    val marker = bytes(4).toChar
    val flag = bytes(5).toChar
    val txInputBytes = bytes.slice(6,bytes.size)
    val inputs = RawTransactionInputParser.read(txInputBytes)
    val inputsSize = inputs.map(_.size).sum

    val outputsStartIndex = inputsSize + 7
    val outputsBytes = bytes.slice(outputsStartIndex, bytes.size)
    val outputs = RawTransactionOutputParser.read(outputsBytes)
    val witnessStartIndex = outputsStartIndex + outputs.map(_.size).sum + 1
    val witnessBytes = bytes.slice(witnessStartIndex,bytes.size)
    logger.info("Witness bytes: " + BitcoinSUtil.encodeHex(witnessBytes))
    val witness = TransactionWitness(witnessBytes, inputs.size)
    val lockTimeStartIndex = witnessStartIndex + witness.bytes.size
    val lockTimeBytes = bytes.slice(lockTimeStartIndex, lockTimeStartIndex + 4)

    val lockTime = UInt32(lockTimeBytes.reverse)

    WitnessTransaction(version,inputs,outputs,lockTime,witness)
  }

  def write(tx: WitnessTransaction): String = {
    val txVersionHex = tx.version.hex
    val version = BitcoinSUtil.flipEndianness(txVersionHex)

    val inputs : String = RawTransactionInputParser.write(tx.inputs)
    val outputs : String = RawTransactionOutputParser.write(tx.outputs)
    val witness = tx.witness.hex
    val lockTimeWithoutPadding : String = tx.lockTime.hex
    val lockTime = addPadding(8,BitcoinSUtil.flipEndianness(lockTimeWithoutPadding))
    version + "0" + "0" + "0" + "1" + inputs + outputs + witness + lockTime
  }
}

object RawWitnessTransactionParser extends RawWitnessTransactionParser
