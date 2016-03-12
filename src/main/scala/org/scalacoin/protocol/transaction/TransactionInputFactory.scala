package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.RawTransactionInputParser
import org.scalacoin.protocol.script.{ScriptSignatureFactory, ScriptSignature, ScriptPubKey}
import org.scalacoin.util.{Factory, ScalacoinUtil}

/**
 * Created by chris on 2/19/16.
 */
trait TransactionInputFactory extends Factory[TransactionInput] { this : TransactionInput =>

  def factory(scriptSig : ScriptSignature) : TransactionInput = {
    TransactionInputImpl(previousOutput,scriptSig,sequence)
  }

  def factory(scriptPubKey: ScriptPubKey) : TransactionInput = {
    val scriptSig = ScriptSignature.fromHex(scriptPubKey.hex)
    factory(scriptSig)
  }

  def factory(sequenceNumber : Long) : TransactionInput = {
    TransactionInputImpl(previousOutput, scriptSignature,sequenceNumber)
  }

  def factory(output : TransactionOutput, outputsTransaction : Transaction) : TransactionInput = {
    val outPoint = TransactionOutPoint.factory(output,outputsTransaction)
    factory(outPoint)
  }

  def factory(outPoint: TransactionOutPoint) : TransactionInput = {
    TransactionInputImpl(outPoint,scriptSignature,sequence)
  }

  def empty : TransactionInput = {
    TransactionInputImpl(TransactionOutPoint.empty,
      ScriptSignature.empty,TransactionConstants.sequence)
  }

  //TODO: This could bomb if the serialized tx input is not in the right format
  //probably should put more thought into this to make it more robust
  def fromBytes(bytes : Seq[Byte]) : TransactionInput = RawTransactionInputParser.read(bytes).head

}
