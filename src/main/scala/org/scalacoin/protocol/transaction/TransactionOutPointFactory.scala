package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.RawTransactionOutPointParser
import org.scalacoin.util.Factory

/**
 * Created by chris on 2/22/16.
 */
trait TransactionOutPointFactory extends Factory[TransactionOutPoint] {

  /**
   * Creates a transaction outpoint from a TransactionOutput & it's Transaction
   * @param output
   * @return
   */
  def factory(output : TransactionOutput,parentTransaction : Transaction) : TransactionOutPoint = {
    val indexOfOutput = parentTransaction.outputs.indexOf(output)
    if (indexOfOutput == -1) throw new RuntimeException("This output is not contained in the parent transaction")
    else TransactionOutPointImpl(parentTransaction.txId,indexOfOutput)
  }

  def factory(txId : String, index : Int) = {
    TransactionOutPointImpl(txId, index)
  }

  def fromBytes(bytes : Seq[Byte]) : TransactionOutPoint = RawTransactionOutPointParser.read(bytes)

}

object TransactionOutPointFactory extends TransactionOutPointFactory

