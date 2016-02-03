package org.bitcoins.protocol.transaction

/**
 * Created by chris on 12/26/15.
 */
trait TransactionOutPoint {
  def txId : String
  def vout : Int
}

case class TransactionOutPointImpl(txId : String, vout : Int) extends TransactionOutPoint