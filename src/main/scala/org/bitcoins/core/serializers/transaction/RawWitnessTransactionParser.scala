package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.EmptyScriptWitness
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.serializers.{ RawBitcoinSerializer, RawSerializerHelper }
import org.bitcoins.core.util.BitcoinSUtil

/**
 * Created by chris on 11/21/16.
 */
sealed abstract class RawWitnessTransactionParser extends RawBitcoinSerializer[WitnessTransaction] {

  /**
   * This read function is unique to [[org.bitcoins.core.serializers.transaction.RawBaseTransactionParser]]
   * in the fact that it reads a 'marker' and 'flag' byte to indicate that this tx is a [[WitnessTransaction]]
   * See BIP144 for more details:
   * [[https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki]]
   * Functionality inside of Bitcoin Core:
   * [[https://github.com/bitcoin/bitcoin/blob/e8cfe1ee2d01c493b758a67ad14707dca15792ea/src/primitives/transaction.h#L244-L251]]
   */
  def read(bytes: List[Byte]): WitnessTransaction = {
    val versionBytes = bytes.take(4)
    val version = UInt32(versionBytes.reverse)
    val marker = bytes(4)
    require(marker.toInt == 0, "Incorrect marker for witness transaction, the marker MUST be 0 for the marker according to BIP141, got: " + marker)
    val flag = bytes(5)
    require(flag.toInt != 0, "Incorrect flag for witness transaction, this must NOT be 0 according to BIP141, got: " + flag)
    val txInputBytes = bytes.slice(6, bytes.size)
    val (inputs, outputBytes) = RawSerializerHelper.parseCmpctSizeUIntSeq(txInputBytes, RawTransactionInputParser.read(_))
    val (outputs, witnessBytes) = RawSerializerHelper.parseCmpctSizeUIntSeq(outputBytes, RawTransactionOutputParser.read(_))
    val witness = TransactionWitness(witnessBytes, inputs.size)
    val lockTimeBytes = witnessBytes.splitAt(witness.size)._2
    val lockTime = UInt32(lockTimeBytes.take(4).reverse)
    WitnessTransaction(version, inputs, outputs, lockTime, witness)
  }

  /**
   * Writes a [[WitnessTransaction]] to a hex string
   * This is unique from [[RawBaseTransactionParser]] in the fact that it adds a 'marker' and 'flag' to indicate
   * that this tx is a [[WitnessTransaction]] and has extra witness data attached to it
   * See BIP144 for more info
   * [[https://github.com/bitcoin/bips/blob/master/bip-0144.mediawiki]]
   * Functionality inside of Bitcoin Core:
   * [[https://github.com/bitcoin/bitcoin/blob/e8cfe1ee2d01c493b758a67ad14707dca15792ea/src/primitives/transaction.h#L282-L287s]]
   */
  def write(tx: WitnessTransaction): Seq[Byte] = {
    val version = tx.version.bytes.reverse
    val inputs = RawSerializerHelper.writeCmpctSizeUInt[TransactionInput](
      tx.inputs,
      RawTransactionInputParser.write(_)
    )
    val outputs = RawSerializerHelper.writeCmpctSizeUInt[TransactionOutput](
      tx.outputs,
      RawTransactionOutputParser.write(_)
    )
    val witness = tx.witness.bytes
    val lockTime = tx.lockTime.bytes.reverse
    //notice we use the old serialization format if all witnesses are empty
    //[[https://github.com/bitcoin/bitcoin/blob/e8cfe1ee2d01c493b758a67ad14707dca15792ea/src/primitives/transaction.h#L276-L281]]
    if (tx.witness.witnesses.exists(_ != EmptyScriptWitness)) {
      val witConstant = Seq(0.toByte, 1.toByte)
      version ++ witConstant ++ inputs ++ outputs ++ witness ++ lockTime
    } else BaseTransaction(tx.version, tx.inputs, tx.outputs, tx.lockTime).bytes
  }
}

object RawWitnessTransactionParser extends RawWitnessTransactionParser
