package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.script.{ EmptyScriptSignature, ScriptSignature }
import org.bitcoins.core.serializers.transaction.RawTransactionInputParser
import org.bitcoins.core.util.Factory

/**
 * Created by chris on 12/26/15.
 * Algebraic data type that represents a transaction input
 */
sealed abstract class TransactionInput extends NetworkElement {

  def previousOutput: TransactionOutPoint

  def scriptSignature: ScriptSignature
  def sequence: UInt32
  override def bytes = RawTransactionInputParser.write(this)
}

case object EmptyTransactionInput extends TransactionInput {
  override def previousOutput = EmptyTransactionOutPoint
  override def scriptSignature = EmptyScriptSignature
  override def sequence = TransactionConstants.sequence
}

/**
 * This represents a coinbase input - these always have a EmptyTransactionOutPoint
 * and arbitrary data inside the script signature
 */
sealed abstract class CoinbaseInput extends TransactionInput {
  override def previousOutput = EmptyTransactionOutPoint
  override def sequence = TransactionConstants.sequence
}

object TransactionInput extends Factory[TransactionInput] {
  private case class TransactionInputImpl(
    previousOutput:  TransactionOutPoint,
    scriptSignature: ScriptSignature, sequence: UInt32
  ) extends TransactionInput
  def empty: TransactionInput = EmptyTransactionInput

  def fromBytes(bytes: Seq[Byte]): TransactionInput = RawTransactionInputParser.read(bytes)

  def apply(outPoint: TransactionOutPoint, scriptSignature: ScriptSignature,
            sequenceNumber: UInt32): TransactionInput = outPoint match {
    case EmptyTransactionOutPoint => CoinbaseInput(scriptSignature)
    case _: TransactionOutPoint   => TransactionInputImpl(outPoint, scriptSignature, sequenceNumber)
  }

}

object CoinbaseInput {

  private case class CoinbaseInputImpl(scriptSignature: ScriptSignature) extends CoinbaseInput
  /**
   * Creates a coinbase input - coinbase inputs always have an empty outpoint
   * @param scriptSignature this can contain anything, miners use this to signify support for various protocol BIPs
   * @return the coinbase input
   */
  def apply(scriptSignature: ScriptSignature): CoinbaseInput = {
    CoinbaseInputImpl(scriptSignature)
  }
}