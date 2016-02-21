package org.scalacoin.protocol.transaction

/**
 * Created by chris on 2/21/16.
 */
trait TransactionFactory { this : Transaction =>

  def factory(outputs : Seq[TransactionOutput]) = {
    TransactionImpl(version,inputs,outputs,lockTime)
  }

}
