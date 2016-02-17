package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.{RawTransactionOutPointParser, TransactionElement}

/**
 * Created by chris on 12/26/15.
 */
trait TransactionOutPoint extends TransactionElement {
  def txId : String
  def vout : Int

  //https://bitcoin.org/en/developer-reference#outpoint
  override def size = 36

  override def hex = RawTransactionOutPointParser.write(this)
}

case class TransactionOutPointImpl(txId : String, vout : Int) extends TransactionOutPoint