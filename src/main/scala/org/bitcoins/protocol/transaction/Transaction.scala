package org.bitcoins.protocol.transaction

import org.bitcoins.marshallers.transaction.{RawTransactionParser, TransactionElement}
import org.bitcoins.util.{Factory, BitcoinSUtil, CryptoUtil}

/**
 * Created by chris on 7/14/15.
 */


sealed trait Transaction extends TransactionElement {
  def txId : String = BitcoinSUtil.encodeHex(CryptoUtil.doubleSHA256(hex).reverse)
  def version : Long
  def inputs  : Seq[TransactionInput]
  def outputs : Seq[TransactionOutput]
  def lockTime : Long

  override def hex = RawTransactionParser.write(this)
}

case object EmptyTransaction extends Transaction {
  override def txId = "0000000000000000000000000000000000000000000000000000000000000000"
  override def version = TransactionConstants.version
  override def inputs = Seq()
  override def outputs = Seq()
  override def lockTime = TransactionConstants.lockTime
}
sealed case class TransactionImpl(version : Long, inputs : Seq[TransactionInput],
  outputs : Seq[TransactionOutput], lockTime : Long) extends Transaction

object Transaction extends Factory[Transaction] {
  /**
    * Updates a transaction outputs
 *
    * @param updatedOutputs
    * @return
    */
  def factory(oldTx : Transaction, updatedOutputs : UpdateTransactionOutputs) : Transaction = {
    TransactionImpl(oldTx.version,oldTx.inputs,updatedOutputs.outputs,oldTx.lockTime)
  }

  /**
    * Updates a transaction's inputs
 *
    * @param updatedInputs
    * @return
    */
  def factory(oldTx : Transaction,updatedInputs : UpdateTransactionInputs) : Transaction = {
    TransactionImpl(oldTx.version,updatedInputs.inputs,oldTx.outputs,oldTx.lockTime)
  }

  /**
    * Factory function that modifies a transactions locktime
 *
    * @param oldTx
    * @param lockTime
    * @return
    */
  def factory(oldTx : Transaction, lockTime : Long) : Transaction = {
    TransactionImpl(oldTx.version,oldTx.inputs,oldTx.outputs,lockTime)
  }


  /**
    * Removes the inputs of the transactions
 *
    * @return
    */
  def emptyInputs(oldTx : Transaction) : Transaction = TransactionImpl(oldTx.version,Seq(),oldTx.outputs,oldTx.lockTime)

  /**
    * Removes the outputs of the transactions
 *
    * @return
    */
  def emptyOutputs(oldTx : Transaction) : Transaction = TransactionImpl(oldTx.version,oldTx.inputs,Seq(),oldTx.lockTime)

  def factory(bytes : Array[Byte]) : Transaction = fromBytes(bytes.toSeq)

  def fromBytes(bytes : Seq[Byte]) : Transaction = RawTransactionParser.read(bytes)

  def apply(bytes : Seq[Byte]) : Transaction = fromBytes(bytes)
  def apply(hex: String) : Transaction = fromHex(hex)
  def apply(bytes : Array[Byte]) : Transaction = factory(bytes)
  def apply(oldTx : Transaction, lockTime : Long)  : Transaction = factory(oldTx,lockTime)
  def apply(oldTx : Transaction, updatedInputs : UpdateTransactionInputs) : Transaction = factory(oldTx, updatedInputs)
  def apply(oldTx : Transaction, updatedOutputs : UpdateTransactionOutputs) : Transaction = factory(oldTx, updatedOutputs)

}