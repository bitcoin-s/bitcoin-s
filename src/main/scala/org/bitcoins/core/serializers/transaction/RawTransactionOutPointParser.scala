package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.util.BitcoinSUtil


/**
 * Source for serialization
 * https://bitcoin.org/en/developer-reference#outpoint
 *
 */
trait RawTransactionOutPointParser extends RawBitcoinSerializer[TransactionOutPoint] {


  override def read(bytes : List[Byte]) : TransactionOutPoint = {
    val txId : List[Byte] = bytes.slice(0,32).reverse
    val index : BigInt = BigInt(bytes.slice(32, bytes.size).toArray.reverse)
    TransactionOutPoint(DoubleSha256Digest(txId), index.toInt)
  }

  def write(outPoint : TransactionOutPoint) : String = {
    val indexHexWithoutPadding : String = addPrecedingZero(outPoint.vout.toHexString)
    val indexHex = addPadding(8,indexHexWithoutPadding)
    val littleEndianTxId = BitcoinSUtil.flipEndianess(outPoint.txId.bytes)
    littleEndianTxId + indexHex
  }

}


object RawTransactionOutPointParser extends RawTransactionOutPointParser
