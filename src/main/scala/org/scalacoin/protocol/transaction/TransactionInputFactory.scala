package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.RawTransactionInputParser
import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.util.{Factory}

/**
 * Created by chris on 2/19/16.
 * Responsible for creating TransactionInputs
 */
trait TransactionInputFactory extends Factory[TransactionInput] {

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
   * @param oldInput
   * @param output
   * @param outputsTransaction
   * @return
   */
  def factory(oldInput : TransactionInput,output : TransactionOutput, outputsTransaction : Transaction) : TransactionInput = {
    val outPoint = TransactionOutPointFactory.factory(output,outputsTransaction)
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

}


object TransactionInputFactory extends TransactionInputFactory
