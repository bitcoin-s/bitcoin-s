package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
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

    val byteNumbers : Seq[BigInt] = for {
      (byte,index) <- indexBytes.zipWithIndex
    } yield NumberUtil.calculateUnsignedNumberFromByte(index, byte)
    logger.debug("Byte numbers: " + byteNumbers)
    val index = byteNumbers.sum
    TransactionOutPoint(DoubleSha256Digest(txId), index.toInt)
  }

  def write(outPoint : TransactionOutPoint) : String = {
    val indexHexWithoutPadding : String = BitcoinSUtil.flipEndianess(BitcoinSUtil.encodeHex(outPoint.vout))
    val indexHex = addPadding(8,indexHexWithoutPadding)
    val txIdHex = outPoint.txId.hex
    txIdHex + indexHex
  }

}


object RawTransactionOutPointParser extends RawTransactionOutPointParser
