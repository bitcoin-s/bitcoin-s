package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{ EmptyTransactionOutPoint, TransactionOutPoint }
import org.bitcoins.core.serializers.RawBitcoinSerializer

/**
 * Source for serialization
 * https://bitcoin.org/en/developer-reference#outpoint
 */
sealed abstract class RawTransactionOutPointParser extends RawBitcoinSerializer[TransactionOutPoint] {

  override def read(bytes: List[Byte]): TransactionOutPoint = {
    val txId: List[Byte] = bytes.take(32)
    val indexBytes = bytes.slice(32, 36)
    val index = UInt32(indexBytes.reverse)
    TransactionOutPoint(DoubleSha256Digest(txId), index)
  }

  def write(outPoint: TransactionOutPoint): Seq[Byte] = {
    //UInt32s cannot hold negative numbers, but sometimes the Bitcoin Protocol requires the vout to be -1, which is serialized
    //as "0xFFFFFFFF".
    //https://github.com/bitcoin/bitcoin/blob/d612837814020ae832499d18e6ee5eb919a87907/src/primitives/transaction.h
    //http://stackoverflow.com/questions/2711522/what-happens-if-i-assign-a-negative-value-to-an-unsigned-variable
    val idxBytes = outPoint match {
      case EmptyTransactionOutPoint      => UInt32.max.bytes
      case outPoint: TransactionOutPoint => outPoint.vout.bytes
    }
    val txIdHex = outPoint.txId.bytes
    txIdHex ++ idxBytes.reverse
  }

}

object RawTransactionOutPointParser extends RawTransactionOutPointParser
