package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.serializers.transaction.RawTransactionOutPointParser
import org.bitcoins.core.util.Factory

/**
 * Created by chris on 12/26/15.
 *
 */
sealed trait TransactionOutPoint extends TransactionElement {
  /**
    * The transaction id for the crediting transaction for this input
 *
    * @return
    */
  def txId : String

  /**
    * The output index in the parent transaction for the output we are spending
 *
    * @return
    */
  def vout : Int

  //https://bitcoin.org/en/developer-reference#outpoint
  override def size = 36

  override def hex = RawTransactionOutPointParser.write(this)
}

case object EmptyTransactionOutPoint extends TransactionOutPoint {
  def txId : String = "0000000000000000000000000000000000000000000000000000000000000000"
  def vout = -1
}

object TransactionOutPoint extends Factory[TransactionOutPoint] {

  private sealed case class TransactionOutPointImpl(txId : String, vout : Int) extends TransactionOutPoint
  /**
    * Creates a transaction outpoint from a TransactionOutput & it's parent transaction
 *
    * @param output
    * @return
    */
  private def factory(output : TransactionOutput, parentTransaction : Transaction) : TransactionOutPoint = {
    val indexOfOutput = parentTransaction.outputs.indexOf(output)
    if (indexOfOutput == -1) throw new RuntimeException("This output is not contained in the parent transaction")
    else factory(parentTransaction.txId,indexOfOutput)
  }

  private def factory(txId : String, index : Int) = {
    if (txId == EmptyTransactionOutPoint.txId && index == EmptyTransactionOutPoint.vout) {
      EmptyTransactionOutPoint
    } else TransactionOutPointImpl(txId, index)
  }

  def fromBytes(bytes : Seq[Byte]) : TransactionOutPoint = RawTransactionOutPointParser.read(bytes)

  def apply(bytes : Seq[Byte]) : TransactionOutPoint = fromBytes(bytes)
  def apply(hex : String) : TransactionOutPoint = fromHex(hex)
  def apply(output : TransactionOutput,parentTransaction : Transaction) : TransactionOutPoint = factory(output,parentTransaction)
  def apply(txId : String, index: Int) : TransactionOutPoint = factory(txId,index)
}

