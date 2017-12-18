package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.serializers.transaction.RawTransactionWitnessParser
import org.bitcoins.core.util.BitcoinSUtil

/**
  * Created by chris on 11/21/16.
  * The witness data for [[org.bitcoins.core.protocol.script.ScriptSignature]] in this transaction
  * [[https://github.com/bitcoin/bitcoin/blob/b4e4ba475a5679e09f279aaf2a83dcf93c632bdb/src/primitives/transaction.h#L232-L268]]
  */
sealed abstract class TransactionWitness extends NetworkElement {
  def witnesses: Seq[ScriptWitness]

  override def bytes = RawTransactionWitnessParser.write(this)
}

/** Used to represent a transaction witness pre segwit, see BIP141 for details */
case object EmptyWitness extends TransactionWitness {
  override def bytes = Seq(0.toByte)
  override def witnesses = Nil
}

object TransactionWitness {
  private case class TransactionWitnessImpl(witnesses: Seq[ScriptWitness]) extends TransactionWitness

  def apply(witnesses: Seq[ScriptWitness]): TransactionWitness = TransactionWitnessImpl(witnesses)

  def fromBytes(bytes: Seq[Byte], numInputs: Int): TransactionWitness = RawTransactionWitnessParser.read(bytes,numInputs)

  def apply(bytes: Seq[Byte], numInputs: Int): TransactionWitness = fromBytes(bytes,numInputs)

  def apply(hex: String, numInputs: Int): TransactionWitness = fromBytes(BitcoinSUtil.decodeHex(hex),numInputs)
}