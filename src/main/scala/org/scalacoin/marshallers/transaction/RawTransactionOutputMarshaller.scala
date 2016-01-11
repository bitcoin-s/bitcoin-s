package org.scalacoin.marshallers.transaction

import org.scalacoin.currency.Satoshis
import org.scalacoin.marshallers.RawBitcoinSerializer
import org.scalacoin.protocol.transaction.TransactionOutput
import org.scalacoin.util.ScalacoinUtil

/**
 * Created by chris on 1/11/16.
 * https://bitcoin.org/en/developer-reference#txout
 */
object RawTransactionOutputMarshaller extends RawBitcoinSerializer[Seq[TransactionOutput]] with ScriptParser {


  override def read(str : String) : Seq[TransactionOutput] = {
    val bytes = ScalacoinUtil.decodeHex(str)

    val numOutputs = bytes(0).toInt
    val satoshisHex = ScalacoinUtil.encodeHex(bytes.slice(1,9))
    val satoshis = Satoshis(Integer.parseInt(satoshisHex,16))
    ???
  }
}
