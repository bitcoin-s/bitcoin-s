package org.bitcoins.protocol.transaction

import org.bitcoins.marshallers.transaction.{RawTransactionInputParser, TransactionElement}
import org.bitcoins.protocol.{CompactSizeUInt}
import org.bitcoins.protocol.script.{ScriptSignature, ScriptPubKey}

import org.bitcoins.util.{Factory, BitcoinSUtil}

/**
 * Created by chris on 12/26/15.
 */
sealed trait TransactionInput extends TransactionElement  {

  def previousOutput : TransactionOutPoint
  def scriptSignature : ScriptSignature
  def sequence : Long


  def scriptSigCompactSizeUInt : CompactSizeUInt = BitcoinSUtil.parseCompactSizeUInt(scriptSignature)
  //https://bitcoin.org/en/developer-reference#txin
  override def size = previousOutput.size + scriptSignature.size +
    scriptSigCompactSizeUInt.size.toInt + 4

  def hex = RawTransactionInputParser.write(Seq(this))
}

case object EmptyTransactionInput extends TransactionInput {
  override def previousOutput = TransactionInput.empty.previousOutput
  override def scriptSignature = TransactionInput.empty.scriptSignature
  override def sequence = TransactionInput.empty.sequence
  override def scriptSigCompactSizeUInt = TransactionInput.empty.scriptSigCompactSizeUInt
}

sealed case class TransactionInputImpl(previousOutput : TransactionOutPoint,
  scriptSignature : ScriptSignature, sequence : Long) extends TransactionInput

object TransactionInput extends Factory[TransactionInput] {
  def factory(oldInput : TransactionInput, scriptSig : ScriptSignature) : TransactionInput = {
    TransactionInputImpl(oldInput.previousOutput,scriptSig,oldInput.sequence)
  }

  def factory(oldInput : TransactionInput, scriptPubKey: ScriptPubKey) : TransactionInput = {
    val scriptSig = ScriptSignature(scriptPubKey.hex)
    factory(oldInput,scriptSig)
  }

  def factory(oldInput : TransactionInput,sequenceNumber : Long) : TransactionInput = {
    TransactionInputImpl(oldInput.previousOutput, oldInput.scriptSignature,sequenceNumber)
  }

  /**
    * Creates a transaction input from a given output and the output's transaction
 *
    * @param oldInput
    * @param output
    * @param outputsTransaction
    * @return
    */
  def factory(oldInput : TransactionInput,output : TransactionOutput, outputsTransaction : Transaction) : TransactionInput = {
    val outPoint = TransactionOutPoint(output,outputsTransaction)
    factory(oldInput,outPoint)
  }

  def factory(oldInput : TransactionInput, outPoint: TransactionOutPoint) : TransactionInput = {
    TransactionInputImpl(outPoint,oldInput.scriptSignature,oldInput.sequence)
  }


  def factory(outPoint : TransactionOutPoint, scriptSignature : ScriptSignature, sequenceNumber : Long) : TransactionInput = {
    TransactionInputImpl(outPoint, scriptSignature, sequenceNumber)
  }

  def empty : TransactionInput = {
    TransactionInputImpl(EmptyTransactionOutPoint,
      ScriptSignature.empty,TransactionConstants.sequence)
  }

  //TODO: This could bomb if the serialized tx input is not in the right format
  //probably should put more thought into this to make it more robust
  def fromBytes(bytes : Seq[Byte]) : TransactionInput = RawTransactionInputParser.read(bytes).head

  def apply(oldInput : TransactionInput, scriptSig : ScriptSignature) : TransactionInput = factory(oldInput,scriptSig)
  def apply(oldInput : TransactionInput, scriptPubKey: ScriptPubKey) : TransactionInput = factory(oldInput, scriptPubKey)
  def apply(oldInput : TransactionInput,sequenceNumber : Long) : TransactionInput = factory(oldInput, sequenceNumber)
  def apply(oldInput : TransactionInput,output : TransactionOutput, outputsTransaction : Transaction) : TransactionInput = factory(oldInput,output,outputsTransaction)
  def apply(oldInput : TransactionInput, outPoint: TransactionOutPoint) : TransactionInput = factory(oldInput,outPoint)
  def apply(outPoint : TransactionOutPoint, scriptSignature : ScriptSignature, sequenceNumber : Long) : TransactionInput = factory(outPoint,scriptSignature,sequenceNumber)
  def apply(bytes : Seq[Byte]) : TransactionInput = fromBytes(bytes)
}