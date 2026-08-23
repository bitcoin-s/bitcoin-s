package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.p2p._
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte
import scodec.bits.ByteVector

sealed abstract class RawFeeFilterMessageSerializer
    extends RawBitcoinSerializer[FeeFilterMessage] {

  override def read(bytes: ByteVector): FeeFilterMessage = {
    // Satoshis(bytes: ByteVector) already reverses (it's read via
    // RawSatoshisSerializer.read, which expects big-endian and reverses a
    // little-endian input); reversing here too was a double-reversal bug
    // that read this little-endian wire field as if big-endian
    val satBytes = bytes.take(8)
    val sat = Satoshis(satBytes)
    val satPerKb = SatoshisPerKiloByte(sat)
    FeeFilterMessage(satPerKb)
  }

  override def write(feeFilterMessage: FeeFilterMessage): ByteVector = {
    // Satoshis.bytes already produces little-endian wire bytes (via
    // RawSatoshisSerializer.write, which reverses its big-endian internal
    // representation); reversing here too was the write-side half of the
    // same double-reversal bug
    feeFilterMessage.feeRate.currencyUnit.satoshis.bytes
  }
}

object RawFeeFilterMessageSerializer extends RawFeeFilterMessageSerializer
