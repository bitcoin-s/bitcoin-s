package org.scalacoin.protocol.transaction

/**
 * Created by chris on 2/21/16.
 */

trait TransactionFactory { this : Transaction =>


  /**
   * Updates a transaction outputs
   * @param updatedOutputs
   * @return
   */
  def factory(updatedOutputs : UpdateTransactionOutputs) : Transaction = {
    TransactionImpl(version,inputs,updatedOutputs.outputs,lockTime)
  }

  /**
   * Updates a transaction's inputs
   * @param updatedInputs
   * @return
   */
  def factory(updatedInputs : UpdateTransactionInputs) : Transaction = {
    TransactionImpl(version,updatedInputs.inputs,outputs,lockTime)
  }


  /**
   * Removes the inputs of the transactions
   * @return
   */
  def emptyInputs : Transaction = TransactionImpl(version,Seq(),outputs,lockTime)

  /**
   * Removes the outputs of the transactions
   * @return
   */
  def emptyOutputs : Transaction = TransactionImpl(version,inputs,Seq(),lockTime)


}

sealed trait TransactionFactoryHelper
case class UpdateTransactionOutputs(outputs : Seq[TransactionOutput]) extends TransactionFactoryHelper
case class UpdateTransactionInputs(inputs : Seq[TransactionInput]) extends TransactionFactoryHelper

