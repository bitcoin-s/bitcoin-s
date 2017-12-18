package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.serializers.transaction.RawTransactionInputParser
import org.bitcoins.core.util.Factory

/**
 * Created by chris on 12/26/15.
  * Algebraic data type that represents a transaction input
 */
sealed trait TransactionInput extends NetworkElement {

  def previousOutput : TransactionOutPoint

  def scriptSignature : ScriptSignature

  def sequence : UInt32

  //https://bitcoin.org/en/developer-reference#txin
  override def size = previousOutput.size + scriptSignature.size + 4

  override def bytes = RawTransactionInputParser.write(this)
}

case object EmptyTransactionInput extends TransactionInput {
  override def previousOutput = TransactionInput.empty.previousOutput
  override def scriptSignature = TransactionInput.empty.scriptSignature
  override def sequence = TransactionInput.empty.sequence
}

/**
  * This represents a coinbase input - these always have a EmptyTransactionOutPoint
  * and arbitrary data inside the script signature
  */
sealed trait CoinbaseInput extends TransactionInput {
  override def previousOutput = EmptyTransactionOutPoint
  override def sequence = TransactionConstants.sequence
}



object TransactionInput extends Factory[TransactionInput] {
  private def factory(oldInput : TransactionInput, scriptSig : ScriptSignature) : TransactionInput = {
    apply(oldInput.previousOutput,scriptSig,oldInput.sequence)
  }
  private sealed case class TransactionInputImpl(previousOutput : TransactionOutPoint,
                                         scriptSignature : ScriptSignature, sequence : UInt32) extends TransactionInput

  private sealed case class CoinbaseInputImpl(
    scriptSignature : ScriptSignature) extends CoinbaseInput

  private def factory(oldInput : TransactionInput, scriptPubKey: ScriptPubKey) : TransactionInput = {
    val scriptSig = ScriptSignature(scriptPubKey.hex)
    factory(oldInput,scriptSig)
  }

  private def factory(oldInput : TransactionInput,sequenceNumber : UInt32) : TransactionInput = {
    TransactionInputImpl(oldInput.previousOutput, oldInput.scriptSignature,sequenceNumber)
  }

  /** Creates a transaction input from a given output and the output's transaction */
  private def factory(oldInput : TransactionInput,output : TransactionOutput, outputsTransaction : Transaction) : TransactionInput = {
    val outPoint = TransactionOutPoint(output,outputsTransaction)
    factory(oldInput,outPoint)
  }

  private def factory(oldInput : TransactionInput, outPoint: TransactionOutPoint) : TransactionInput = {
    TransactionInputImpl(outPoint,oldInput.scriptSignature,oldInput.sequence)
  }


  private def factory(outPoint : TransactionOutPoint, scriptSignature : ScriptSignature, sequenceNumber : UInt32) : TransactionInput = {
    outPoint match {
      case EmptyTransactionOutPoint => CoinbaseInputImpl(scriptSignature)
      case _ : TransactionOutPoint => TransactionInputImpl(outPoint, scriptSignature, sequenceNumber)
    }
  }

  def empty : TransactionInput = {
    TransactionInputImpl(EmptyTransactionOutPoint,
      ScriptSignature.empty,TransactionConstants.sequence)
  }

  def fromBytes(bytes : Seq[Byte]) : TransactionInput = RawTransactionInputParser.read(bytes)

  def apply(oldInput : TransactionInput, scriptSig : ScriptSignature) : TransactionInput = factory(oldInput,scriptSig)

  def apply(oldInput : TransactionInput, script : Seq[ScriptToken]) : TransactionInput = {
    val scriptSig = ScriptSignature.fromAsm(script)
    apply(oldInput,scriptSig)
  }

  def apply(oldInput : TransactionInput, scriptPubKey: ScriptPubKey) : TransactionInput = factory(oldInput, scriptPubKey)

  def apply(oldInput : TransactionInput,sequenceNumber : UInt32) : TransactionInput = factory(oldInput, sequenceNumber)

  def apply(oldInput : TransactionInput,output : TransactionOutput, outputsTransaction : Transaction) : TransactionInput = factory(oldInput,output,outputsTransaction)

  def apply(oldInput : TransactionInput, outPoint: TransactionOutPoint) : TransactionInput = factory(oldInput,outPoint)

  def apply(outPoint : TransactionOutPoint, scriptSignature : ScriptSignature, sequenceNumber : UInt32) : TransactionInput = factory(outPoint,scriptSignature,sequenceNumber)

  /**
    * Creates a coinbase input - coinbase inputs always have an empty outpoint
    * @param scriptSignature this can contain anything, miners use this to signify support for various protocol BIPs
    * @return the coinbase input
    */
  def apply(scriptSignature: ScriptSignature) : CoinbaseInput = {
    CoinbaseInputImpl(scriptSignature)
  }
}