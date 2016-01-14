package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.TransactionElement

/**
 * Created by chris on 12/26/15.
 */
trait TransactionOutPoint extends TransactionElement {
  def txId : String
  def vout : Int

  //https://bitcoin.org/en/developer-reference#outpoint
  def size = 36
}

case class TransactionOutPointImpl(txId : String, vout : Int) extends TransactionOutPoint