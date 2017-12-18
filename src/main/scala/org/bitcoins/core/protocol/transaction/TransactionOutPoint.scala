package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.serializers.transaction.RawTransactionOutPointParser
import org.bitcoins.core.util.{BitcoinSUtil, Factory}
/**
 * Created by chris on 12/26/15.
 *
 */
sealed trait TransactionOutPoint extends NetworkElement {
  /** The transaction id for the crediting transaction for this input */
  def txId : DoubleSha256Digest

  /** The output index in the parent transaction for the output we are spending */
  def vout : UInt32

  //https://bitcoin.org/en/developer-reference#outpoint
  override def size = 36

  override def bytes = RawTransactionOutPointParser.write(this)
}

/**
  * UInt32s cannot hold negative numbers, but sometimes the Bitcoin Protocol requires the vout to be -1, which is serialized
  * as "0xFFFFFFFF".
  * https://github.com/bitcoin/bitcoin/blob/d612837814020ae832499d18e6ee5eb919a87907/src/primitives/transaction.h
  * http://stackoverflow.com/questions/2711522/what-happens-if-i-assign-a-negative-value-to-an-unsigned-variable
  */
case object EmptyTransactionOutPoint extends TransactionOutPoint {
  def txId = DoubleSha256Digest(
    BitcoinSUtil.decodeHex("0000000000000000000000000000000000000000000000000000000000000000"))
  def vout =  UInt32("ffffffff")
}

object TransactionOutPoint extends Factory[TransactionOutPoint] {

  private sealed case class TransactionOutPointImpl(txId : DoubleSha256Digest, vout : UInt32) extends TransactionOutPoint
  /**
    * Creates a transaction outpoint from a TransactionOutput & it's parent transaction
 *
    * @param output
    * @return
    */
  private def factory(output : TransactionOutput, parentTransaction : Transaction) : TransactionOutPoint = {
    val indexOfOutput = UInt32(parentTransaction.outputs.indexOf(output))
    if (indexOfOutput.toInt == (-1)) throw new RuntimeException("This output is not contained in the parent transaction")
    else factory(parentTransaction.txId,indexOfOutput)
  }

  private def factory(txId : DoubleSha256Digest, index : UInt32) = {
    if (txId == EmptyTransactionOutPoint.txId && index == EmptyTransactionOutPoint.vout) {
      EmptyTransactionOutPoint
    } else TransactionOutPointImpl(txId, index)
  }

  def fromBytes(bytes : Seq[Byte]) : TransactionOutPoint = RawTransactionOutPointParser.read(bytes)

  def apply(output : TransactionOutput,parentTransaction : Transaction) : TransactionOutPoint = factory(output,parentTransaction)

  def apply(txId : DoubleSha256Digest, index: UInt32) : TransactionOutPoint = factory(txId,index)
}

