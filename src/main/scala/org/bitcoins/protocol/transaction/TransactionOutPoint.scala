package org.bitcoins.protocol.transaction

import org.bitcoins.marshallers.transaction.{RawTransactionOutPointParser, TransactionElement}
import org.bitcoins.util.Factory

/**
 * Created by chris on 12/26/15.
 *
 */
sealed trait TransactionOutPoint extends TransactionElement {
  def txId : String
  def vout : Int

  //https://bitcoin.org/en/developer-reference#outpoint
  override def size = 36

  override def hex = RawTransactionOutPointParser.write(this)
}

case object EmptyTransactionOutPoint extends TransactionOutPoint {
  def txId : String = ""
  def vout = -1
}

object TransactionOutPoint extends Factory[TransactionOutPoint] {

  /**
    * Creates a transaction outpoint from a TransactionOutput & it's Transaction
 *
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

  def apply(bytes : Seq[Byte]) : TransactionOutPoint = fromBytes(bytes)
  def apply(hex : String) : TransactionOutPoint = fromHex(hex)
  def apply(output : TransactionOutput,parentTransaction : Transaction) : TransactionOutPoint = factory(output,parentTransaction)
  def apply(txId : String, index: Int) : TransactionOutPoint = factory(txId,index)
}

sealed case class TransactionOutPointImpl(txId : String, vout : Int) extends TransactionOutPoint