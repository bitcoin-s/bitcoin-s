package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.serializers.transaction.RawTransactionOutPointParser
import org.bitcoins.crypto.{
  DoubleSha256Digest,
  DoubleSha256DigestBE,
  Factory,
  NetworkElement
}
import scodec.bits._

/**
  * @param txId The transaction id for the crediting transaction for this input
  * @param vout The output index in the parent transaction for the output we are spending
  */
case class TransactionOutPoint(txId: DoubleSha256Digest, vout: UInt32)
    extends NetworkElement {

  def txIdBE: DoubleSha256DigestBE = txId.flip

  override def bytes = RawTransactionOutPointParser.write(this)

  override def toString: String =
    s"TransactionOutPoint(${txIdBE.hex}:${vout.toBigInt})"

  def ==(outPoint: TransactionOutPoint): Boolean =
    txId == outPoint.txId && vout == outPoint.vout

  def !=(outPoint: TransactionOutPoint): Boolean =
    !(this == outPoint)
}

/**
  * UInt32s cannot hold negative numbers, but sometimes the Bitcoin Protocol
  * requires the vout to be -1, which is serialized as `0xFFFFFFFF`.
  *
  * @see [[https://github.com/bitcoin/bitcoin/blob/d612837814020ae832499d18e6ee5eb919a87907/src/primitives/transaction.h transaction.h]]
  * @see http://stackoverflow.com/questions/2711522/what-happens-if-i-assign-a-negative-value-to-an-unsigned-variable
  */
final object EmptyTransactionOutPoint
    extends TransactionOutPoint(txId = DoubleSha256Digest.empty,
                                vout = UInt32.max) {
  override def toString(): String = "EmptyTransactionOutPoint"
}

object TransactionOutPoint extends Factory[TransactionOutPoint] {

  def fromBytes(bytes: ByteVector): TransactionOutPoint =
    RawTransactionOutPointParser.read(bytes)

  /**
    * @param txId The transaction id for the crediting transaction for this input
    * @param vout The output index in the parent transaction for the output we are spending
    */
  def apply(txId: DoubleSha256DigestBE, vout: UInt32): TransactionOutPoint = {
    TransactionOutPoint(txId.flip, vout)
  }
}
