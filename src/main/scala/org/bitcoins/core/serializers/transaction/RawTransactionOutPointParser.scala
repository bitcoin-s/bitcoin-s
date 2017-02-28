package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.protocol.transaction.{EmptyTransactionOutPoint, TransactionOutPoint}
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, NumberUtil}


/**
 * Source for serialization
 * https://bitcoin.org/en/developer-reference#outpoint
 *
 */
trait RawTransactionOutPointParser extends RawBitcoinSerializer[TransactionOutPoint] with BitcoinSLogger {


  override def read(bytes : List[Byte]) : TransactionOutPoint = {
    val txId : List[Byte] = bytes.slice(0,32)
    val indexBytes = bytes.slice(32, bytes.size)
    logger.debug("Index bytes: " + BitcoinSUtil.encodeHex(indexBytes.reverse))

    val index = UInt32(indexBytes.reverse)
    TransactionOutPoint(DoubleSha256Digest(txId), index)
  }

  def write(outPoint : TransactionOutPoint) : String = {
    val indexHexWithoutPadding : String = BitcoinSUtil.flipEndianness(outPoint.vout.hex)
    //UInt32s cannot hold negative numbers, but sometimes the Bitcoin Protocol requires the vout to be -1, which is serialized
    //as "0xFFFFFFFF".
    //https://github.com/bitcoin/bitcoin/blob/d612837814020ae832499d18e6ee5eb919a87907/src/primitives/transaction.h
    //http://stackoverflow.com/questions/2711522/what-happens-if-i-assign-a-negative-value-to-an-unsigned-variable
    val indexHex = outPoint match {
      case EmptyTransactionOutPoint => "ffffffff"
      case outPoint : TransactionOutPoint => addPadding(8,indexHexWithoutPadding)
    }
    val txIdHex = outPoint.txId.hex
    txIdHex + indexHex
  }

}


object RawTransactionOutPointParser extends RawTransactionOutPointParser
