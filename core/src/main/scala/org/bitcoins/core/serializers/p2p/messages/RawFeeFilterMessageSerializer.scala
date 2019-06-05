package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.Int64
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte
import org.bitcoins.core.p2p._
import scodec.bits.ByteVector

sealed abstract class RawFeeFilterMessageSerializer
    extends RawBitcoinSerializer[FeeFilterMessage] {

  override def read(bytes: ByteVector): FeeFilterMessage = {
    val i64 = Int64.fromBytes(bytes.take(8).reverse)
    val sat = Satoshis(i64)
    val satPerKb = SatoshisPerKiloByte(sat)
    FeeFilterMessage(satPerKb)
  }

  override def write(feeFilterMessage: FeeFilterMessage): ByteVector = {
    feeFilterMessage.feeRate.currencyUnit.bytes.reverse
  }
}

object RawFeeFilterMessageSerializer extends RawFeeFilterMessageSerializer
