package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.serializers.transaction.RawTransactionOutputParser
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

case class TransactionOutput(value: CurrencyUnit, scriptPubKey: ScriptPubKey)
    extends NetworkElement {

  //https://bitcoin.org/en/developer-reference#txout
  override lazy val byteSize = scriptPubKey.byteSize + 8

  override def bytes = RawTransactionOutputParser.write(this)
}

final object EmptyTransactionOutput
    extends TransactionOutput(CurrencyUnits.negativeSatoshi, ScriptPubKey.empty) {
  override def toString(): String = "EmptyTransactionOutput"
}

object TransactionOutput extends Factory[TransactionOutput] {

  def fromBytes(bytes: ByteVector): TransactionOutput =
    RawTransactionOutputParser.read(bytes)

}
