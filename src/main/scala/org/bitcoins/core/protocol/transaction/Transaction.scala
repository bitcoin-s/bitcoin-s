package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.transaction.RawTransactionParser
import org.bitcoins.core.util.{Factory, BitcoinSUtil, CryptoUtil}

/**
 * Created by chris on 7/14/15.
 */


sealed trait Transaction extends NetworkElement {
  /**
    * The sha256(sha256(tx)) of this transaction
    * Note that this is the little endian encoding of the hash NOT the big endian encoding
    * which bitcoin core uses
    * @return
    */
  def txId : DoubleSha256Digest = DoubleSha256Digest(CryptoUtil.doubleSHA256(bytes).bytes.reverse)

  /**
    * The version number for this transaction
 *
    * @return
    */
  def version : Long

  /**
    * The inputs for this transaction
 *
    * @return
    */
  def inputs  : Seq[TransactionInput]

  /**
    * The outputs for this transaction
 *
    * @return
    */
  def outputs : Seq[TransactionOutput]

  /**
    * The locktime for this transaction
 *
    * @return
    */
  def lockTime : Long

  override def hex = RawTransactionParser.write(this)

  /**
    * Determines if this transaction is a coinbase transaction
 *
    * @return
    */
  def isCoinbase : Boolean = inputs.size match {
    case 1 => inputs.head match {
      case coinbase : CoinbaseInput => true
      case _ : TransactionInput => false
    }
    case _ : Int => false
  }
}

case object EmptyTransaction extends Transaction {
  override def txId = DoubleSha256Digest(BitcoinSUtil.decodeHex("0000000000000000000000000000000000000000000000000000000000000000"))
  override def version = TransactionConstants.version
  override def inputs = Seq()
  override def outputs = Seq()
  override def lockTime = TransactionConstants.lockTime
}


object Transaction extends Factory[Transaction] {

  private sealed case class TransactionImpl(version : Long, inputs : Seq[TransactionInput],
    outputs : Seq[TransactionOutput], lockTime : Long) extends Transaction
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

  def apply(bytes : Array[Byte]) : Transaction = factory(bytes)
  def apply(oldTx : Transaction, lockTime : Long)  : Transaction = factory(oldTx,lockTime)
  def apply(oldTx : Transaction, updatedInputs : UpdateTransactionInputs) : Transaction = factory(oldTx, updatedInputs)
  def apply(oldTx : Transaction, updatedOutputs : UpdateTransactionOutputs) : Transaction = factory(oldTx, updatedOutputs)

  def apply(version : Int, inputs : Seq[TransactionInput],
            outputs : Seq[TransactionOutput], lockTime : Long) : Transaction = {
    TransactionImpl(version,inputs,outputs,lockTime)
  }
}