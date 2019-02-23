package org.bitcoins.node.messages.control

import org.bitcoins.core.util.Factory
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerKiloByte}
import org.bitcoins.node.messages.FeeFilterMessage
import org.bitcoins.node.serializers.messages.control.RawFeeFilterMessageSerializer
import scodec.bits.ByteVector

object FeeFilterMessage extends Factory[FeeFilterMessage] {


  private case class FeeFilterMessageImpl(feeRate: SatoshisPerKiloByte) extends FeeFilterMessage


  override def fromBytes(bytes: ByteVector): FeeFilterMessage = {
    RawFeeFilterMessageSerializer.read(bytes)
  }


  def apply(satoshisPerKiloByte: SatoshisPerKiloByte): FeeFilterMessage = {
    FeeFilterMessageImpl(satoshisPerKiloByte)
  }


  def apply(satPerByte: SatoshisPerByte): FeeFilterMessage = {
    FeeFilterMessage(satPerByte.toSatPerKb)
  }
}
