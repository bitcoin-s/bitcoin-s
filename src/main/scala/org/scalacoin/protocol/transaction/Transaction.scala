package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.{RawTransactionParser, TransactionElement}
import org.scalacoin.util.{BitcoinSUtil, CryptoUtil}

/**
 * Created by chris on 7/14/15.
 */


sealed trait Transaction extends TransactionElement {
  def txId : String = BitcoinSUtil.encodeHex(CryptoUtil.doubleSHA256(hex).reverse)
  def version : Long
  def inputs  : Seq[TransactionInput]
  def outputs : Seq[TransactionOutput]
  def lockTime : Long

  override def hex = RawTransactionParser.write(this)
}

case object EmptyTransaction extends Transaction {
  override def txId = "0000000000000000000000000000000000000000000000000000000000000000"
  override def version = TransactionConstants.version
  override def inputs = Seq()
  override def outputs = Seq()
  override def lockTime = TransactionConstants.lockTime
}
sealed case class TransactionImpl(version : Long, inputs : Seq[TransactionInput],
  outputs : Seq[TransactionOutput], lockTime : Long) extends Transaction










