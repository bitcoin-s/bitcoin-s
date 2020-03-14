package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte
import org.bitcoins.core.p2p._
import scodec.bits.ByteVector

sealed abstract class RawFeeFilterMessageSerializer
    extends RawBitcoinSerializer[FeeFilterMessage] {

  override def read(bytes: ByteVector): FeeFilterMessage = {
    val satBytes = bytes.take(8).reverse
    val sat = Satoshis(satBytes)
    val satPerKb = SatoshisPerKiloByte(sat.toDouble)
    FeeFilterMessage(satPerKb)
  }

  override def write(feeFilterMessage: FeeFilterMessage): ByteVector = {
    val sats = Satoshis(feeFilterMessage.feeRate.sats.toLong)
    sats.bytes.reverse
  }
}

object RawFeeFilterMessageSerializer extends RawFeeFilterMessageSerializer
