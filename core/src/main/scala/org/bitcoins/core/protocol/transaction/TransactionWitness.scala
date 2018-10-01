package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.script.{ EmptyScriptWitness, ScriptWitness }
import org.bitcoins.core.serializers.transaction.RawTransactionWitnessParser
import org.bitcoins.core.util.BitcoinSUtil
import scodec.bits.ByteVector

/**
 * Created by chris on 11/21/16.
 * The witness data for [[org.bitcoins.core.protocol.script.ScriptSignature]] in this transaction
 * [[https://github.com/bitcoin/bitcoin/blob/b4e4ba475a5679e09f279aaf2a83dcf93c632bdb/src/primitives/transaction.h#L232-L268]]
 */
sealed abstract class TransactionWitness extends NetworkElement {
  def witnesses: Vector[ScriptWitness]

  override def bytes = RawTransactionWitnessParser.write(this)

  /**
   * Update the [[witnesses]] index to the given witness
   * Pads the witnesses vector if needed to accomodate the new witness
   */
  def updated(index: Int, witness: ScriptWitness): TransactionWitness = {
    val scriptWits = {
      if (index < 0) {
        throw new IndexOutOfBoundsException(index.toString)
      } else if (index >= witnesses.size) {

        val padded = padScriptWitness(index)

        padded.updated(index, witness)

      } else {
        witnesses.updated(index, witness)
      }
    }

    TransactionWitness(scriptWits)
  }

  /**
   * Pads the existing [[witnesses]] so that we can insert a witness
   * into the given index. If [[witnesses]] is not properly padded
   * we can have an index out of bounds exception thrown
   * when trying to update the vector.
   */
  private def padScriptWitness(index: Int): Vector[ScriptWitness] = {
    val neededPadding = index - witnesses.size + 1
    if (neededPadding > 0) {
      val emptyWits = Vector.fill(neededPadding)(EmptyScriptWitness)
      witnesses ++ emptyWits
    } else {
      witnesses
    }

  }
}

/** Used to represent a transaction witness pre segwit, see BIP141 for details */
case object EmptyWitness extends TransactionWitness {
  override def bytes = ByteVector.low(1)
  override def witnesses: Vector[ScriptWitness] = Vector.empty

}

object TransactionWitness {
  private case class TransactionWitnessImpl(witnesses: Vector[ScriptWitness]) extends TransactionWitness

  def apply(witnesses: Vector[ScriptWitness]): TransactionWitness = {
    if (witnesses.exists(_ != EmptyScriptWitness)) {
      TransactionWitnessImpl(witnesses)
    } else {
      EmptyWitness
    }
  }
  /**
   * Creates a [[TransactionWitness]] from a Seq[Option[ScriptWitness]].
   * This constructor is for convinience if a certain input does not spend a [[org.bitcoins.core.protocol.script.WitnessScriptPubKey]]
   * It simply transforms the `None` types to [[EmptyScriptWitness]] and then calls the normal TransactionWitness constructor
   * @param witnesses
   * @return
   */
  def fromWitOpt(witnesses: Vector[Option[ScriptWitness]]): TransactionWitness = {
    val replaced: Vector[ScriptWitness] = witnesses.map {
      case Some(wit) => wit
      case None => EmptyScriptWitness
    }
    TransactionWitness(replaced)
  }
  def fromBytes(bytes: ByteVector, numInputs: Int): TransactionWitness = RawTransactionWitnessParser.read(bytes, numInputs)

  def apply(bytes: ByteVector, numInputs: Int): TransactionWitness = fromBytes(bytes, numInputs)

  def apply(hex: String, numInputs: Int): TransactionWitness = fromBytes(BitcoinSUtil.decodeHex(hex), numInputs)
}
