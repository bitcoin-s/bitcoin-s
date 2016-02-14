package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.{RawTransactionParser, TransactionElement}
import org.scalacoin.util.{ScalacoinUtil, CryptoUtil}

/**
 * Created by chris on 7/14/15.
 */


trait Transaction extends TransactionElement {
  def txId : String = ScalacoinUtil.encodeHex(CryptoUtil.doubleSHA256(hex).reverse)
  def version : Long
  def inputs  : Seq[TransactionInput]
  def outputs : Seq[TransactionOutput]
  def lockTime : Long

  def inputsSize = inputs.map(_.size).sum
  def outputsSize = outputs.map(_.size).sum


  //https://bitcoin.org/en/developer-reference#raw-transaction-format
  override def size = 4 + inputsSize + outputsSize  + 4

  override def hex = RawTransactionParser.write(this)
}

case class TransactionImpl(version : Long, inputs : Seq[TransactionInput],
  outputs : Seq[TransactionOutput], lockTime : Long) extends Transaction










