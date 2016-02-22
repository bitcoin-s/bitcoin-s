package org.scalacoin.protocol.transaction

/**
 * Created by chris on 2/22/16.
 */
trait TransactionOutPointFactory { this : TransactionOutPoint =>

  /**
   * Creates a transaction outpoint from a TransactionOutput & it's Transaction
   * @param output
   * @return
   */
  def factory(output : TransactionOutput,parentTransaction : Transaction) : TransactionOutPoint = {
    TransactionOutPointImpl(parentTransaction.txId,output.n)
  }

  def empty : TransactionOutPoint = TransactionOutPointImpl("",-1)
}

