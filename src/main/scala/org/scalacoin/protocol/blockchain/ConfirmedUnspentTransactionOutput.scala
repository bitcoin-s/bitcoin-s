package org.scalacoin.protocol.blockchain

/**
 * Created by Tom on 1/11/2016.
 */
trait ConfirmedUnspentTransactionOutput {
  def height : Int
  def bestBlock : String
  def transactions : Int
  def txOuts : Int
  def bytesSerialized : Int
  def hashSerialized : String
  def totalAmount : Double
}

case class ConfirmedUnspentTransactionOutputImpl(height: Int, bestBlock : String, transactions : Int, txOuts : Int, bytesSerialized : Int,
               hashSerialized : String, totalAmount: Double) extends ConfirmedUnspentTransactionOutput
