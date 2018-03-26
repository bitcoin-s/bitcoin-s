package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.currency.{ CurrencyUnits, Satoshis }
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.serializers.script.RawScriptPubKeyParser
import org.bitcoins.core.serializers.{ RawBitcoinSerializer, RawSatoshisSerializer }

/**
 * Created by chris on 1/11/16.
 * https://bitcoin.org/en/developer-reference#txout
 */
sealed abstract class RawTransactionOutputParser extends RawBitcoinSerializer[TransactionOutput] {

  /** Writes a single transaction output */
  override def write(output: TransactionOutput): Seq[Byte] = {
    val satoshis: Satoshis = CurrencyUnits.toSatoshis(output.value)
    satoshis.bytes ++ output.scriptPubKey.bytes
  }

  /**
   * Reads a single output from the given bytes, note this is different than [[org.bitcoins.core.serializers.transaction.RawTransactionOutputParser.read]]
   * because it does NOT expect a [[CompactSizeUInt]] to be the first element in the byte array indicating how many outputs we have
   */
  override def read(bytes: List[Byte]): TransactionOutput = {
    val satoshisBytes = bytes.take(8)
    val satoshis = RawSatoshisSerializer.read(satoshisBytes)
    //it doesn't include itself towards the size, thats why it is incremented by one
    val scriptPubKeyBytes = bytes.slice(8, bytes.size)
    val scriptPubKey = RawScriptPubKeyParser.read(scriptPubKeyBytes)
    val parsedOutput = TransactionOutput(satoshis, scriptPubKey)
    parsedOutput
  }

}

object RawTransactionOutputParser extends RawTransactionOutputParser