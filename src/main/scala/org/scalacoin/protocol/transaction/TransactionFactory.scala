package org.scalacoin.protocol.transaction

/**
 * Created by chris on 2/21/16.
 */

trait TransactionFactory { this : Transaction =>


  def factory(updatedOutputs : UpdateTransactionOutputs) : Transaction = {
    TransactionImpl(version,inputs,updatedOutputs.outputs,lockTime)
  }

  def factory(updatedInputs : UpdateTransactionInputs) = {
    TransactionImpl(version,updatedInputs.inputs,outputs,lockTime)
  }


}

sealed trait TransactionFactoryHelper
case class UpdateTransactionOutputs(outputs : Seq[TransactionOutput]) extends TransactionFactoryHelper
case class UpdateTransactionInputs(inputs : Seq[TransactionInput]) extends TransactionFactoryHelper

