package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.p2p._
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte
import scodec.bits.ByteVector

sealed abstract class RawFeeFilterMessageSerializer
    extends RawBitcoinSerializer[FeeFilterMessage] {

  override def read(bytes: ByteVector): FeeFilterMessage = {
    val satBytes = bytes.take(8).reverse
    val sat = Satoshis(satBytes)
    val satPerKb = SatoshisPerKiloByte(sat)
    FeeFilterMessage(satPerKb)
  }

  override def write(feeFilterMessage: FeeFilterMessage): ByteVector = {
    feeFilterMessage.feeRate.currencyUnit.satoshis.bytes.reverse
  }
}

object RawFeeFilterMessageSerializer extends RawFeeFilterMessageSerializer
