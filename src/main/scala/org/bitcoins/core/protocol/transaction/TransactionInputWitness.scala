package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.serializers.transaction.RawTransactionInputWitnessParser
import org.bitcoins.core.util.Factory

/**
  * Created by chris on 11/21/16.
  * Represents the witness used to evaluate the [[org.bitcoins.core.protocol.script.ScriptSignature]] or
  * [[org.bitcoins.core.protocol.script.ScriptPubKey]] inside of a [[TransactionInput]]
  * [[https://github.com/bitcoin/bitcoin/blob/b4e4ba475a5679e09f279aaf2a83dcf93c632bdb/src/primitives/transaction.h#L214-L230]]
  */
sealed trait TransactionInputWitness extends NetworkElement {
  /** The [[org.bitcoins.core.protocol.script.ScriptWitness]] used for the evaluation
    * of a [[org.bitcoins.core.protocol.script.ScriptSignature]] or a [[org.bitcoins.core.protocol.script.ScriptPubKey]]
    * @return
    */
  def witness: ScriptWitness

  override def hex = RawTransactionInputWitnessParser.write(this)
}

object TransactionInputWitness extends Factory[TransactionInputWitness] {
  private case class TransactionInputWitnessImpl(witness: ScriptWitness) extends TransactionInputWitness

  override def fromBytes(bytes: Seq[Byte]): TransactionInputWitness = RawTransactionInputWitnessParser.read(bytes)

  def apply(witness: ScriptWitness): TransactionInputWitness = TransactionInputWitnessImpl(witness)
}
