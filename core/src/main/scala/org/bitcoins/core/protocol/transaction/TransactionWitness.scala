package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.script.{EmptyScriptWitness, ScriptWitness}
import org.bitcoins.core.serializers.transaction.RawTransactionWitnessParser
import org.bitcoins.core.util.{BytesUtil, SeqWrapper}
import org.bitcoins.crypto.NetworkElement
import scodec.bits.ByteVector

/**
  * Created by chris on 11/21/16.
  * The witness data for [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]] in this transaction
  * [[https://github.com/bitcoin/bitcoin/blob/b4e4ba475a5679e09f279aaf2a83dcf93c632bdb/src/primitives/transaction.h#L232-L268]]
  */
sealed abstract class TransactionWitness
    extends SeqWrapper[ScriptWitness]
    with NetworkElement {
  val witnesses: Vector[ScriptWitness]
  override protected val wrapped: Vector[ScriptWitness] = witnesses

  override def bytes: ByteVector = {
    RawTransactionWitnessParser.write(this)
  }

  /**
    * Update the `witnesses` index to the given witness
    * Pads the witnesses vector if needed to accomodate the new witness
    */
  def updated(index: Int, witness: ScriptWitness): TransactionWitness = {
    val scriptWits: Vector[ScriptWitness] = {
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
    * Pads the existing `witnesses` so that we can insert a witness
    * into the given index. If `witnesses` is not properly padded
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

/** Each input (even if it does not spend a segwit output) needs to have a witness associated with it
  * in a [[WitnessTransaction]]. This helper case class is used to "fill in" [[EmptyScriptWitness]] for
  * the inputs that do not spend a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 WitnessScriptPubKeyV0]]
  *
  * @see https://github.com/bitcoin/bips/blob/master/bip-0141.mediawiki#specification
  * */
case class EmptyWitness(witnesses: Vector[EmptyScriptWitness.type])
    extends TransactionWitness

object EmptyWitness {

  /** Generates an empty witness with n [[EmptyScriptWitness]] inside it */
  def fromN(n: Int): EmptyWitness = {
    val wits = Vector.fill(n)(EmptyScriptWitness)
    new EmptyWitness(wits)
  }

  def fromInputs(inputs: Seq[TransactionInput]): EmptyWitness = {
    fromN(inputs.length)
  }
}

object TransactionWitness {
  private case class TransactionWitnessImpl(witnesses: Vector[ScriptWitness])
      extends TransactionWitness

  def apply(witnesses: Vector[ScriptWitness]): TransactionWitness = {
    if (witnesses.exists(_ != EmptyScriptWitness)) {
      TransactionWitnessImpl(witnesses)
    } else {
      //means that everything must be a empty ScriptWitness
      EmptyWitness.fromN(witnesses.length)
    }
  }

  /**
    * Creates a [[org.bitcoins.core.protocol.transaction.TransactionWitness TransactionWitness]] from a
    * [[org.bitcoins.core.protocol.script.ScriptWitness Seq[Option[ScriptWitness]].
    * This constructor is for convinience if a certain input does not spend a
    * [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    * It simply transforms the `None` types to
    * [[org.bitcoins.core.protocol.script.EmptyScriptWitness EmptyScriptWitness]] and then calls the normal
    * [[org.bitcoins.core.protocol.transaction.TransactionWitness  TransactionWitness]] constructor
    */
  def fromWitOpt(
      witnesses: Vector[Option[ScriptWitness]]): TransactionWitness = {
    val replaced: Vector[ScriptWitness] = witnesses.map {
      case Some(wit) => wit
      case None      => EmptyScriptWitness
    }
    TransactionWitness(replaced)
  }

  def fromBytes(bytes: ByteVector, numInputs: Int): TransactionWitness =
    RawTransactionWitnessParser.read(bytes, numInputs)

  def apply(bytes: ByteVector, numInputs: Int): TransactionWitness =
    fromBytes(bytes, numInputs)

  def apply(hex: String, numInputs: Int): TransactionWitness =
    fromBytes(BytesUtil.decodeHex(hex), numInputs)
}
