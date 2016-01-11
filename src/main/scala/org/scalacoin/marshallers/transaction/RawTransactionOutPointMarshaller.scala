package org.scalacoin.marshallers.transaction

import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.transaction.{TransactionOutPointImpl, TransactionOutPoint}
import org.scalacoin.util.ScalacoinUtil

/**
 * Source for serialization
 * https://bitcoin.org/en/developer-reference#outpoint
 *
 */
object RawTransactionOutPointMarshaller extends RawBitcoinSerializer[TransactionOutPoint] {


  def read(str : String) : TransactionOutPoint = {
    val bytes = ScalacoinUtil.decodeHex(str)
    val txId : List[Byte] = bytes.slice(0,16)
    val index : BigInt = BigInt(bytes.slice(16, bytes.size).toArray)
    TransactionOutPointImpl(ScalacoinUtil.encodeHex(txId), index.toInt)
  }

  def write(outPoint : TransactionOutPoint) : String = {
    val indexBytes : List[Byte] = List(0x00,0x00,0x00,outPoint.vout.toByte)
    outPoint.txId + ScalacoinUtil.encodeHex(indexBytes)
  }

}
