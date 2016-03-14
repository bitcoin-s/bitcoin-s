package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.RawTransactionOutPointParser
import org.scalacoin.util.Factory

/**
 * Created by chris on 2/22/16.
 */
trait TransactionOutPointFactory extends Factory[TransactionOutPoint] { this : TransactionOutPoint =>

  /**
   * Creates a transaction outpoint from a TransactionOutput & it's Transaction
   * @param output
   * @return
   */
  def factory(output : TransactionOutput,parentTransaction : Transaction) : TransactionOutPoint = {
    TransactionOutPointImpl(parentTransaction.txId,output.n)
  }

  def empty : TransactionOutPoint = TransactionOutPointImpl("",-1)

  def fromBytes(bytes : Seq[Byte]) : TransactionOutPoint = RawTransactionOutPointParser.read(bytes)

}

