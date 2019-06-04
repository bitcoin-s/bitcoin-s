package org.bitcoins.node.messages.data

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.GetDataMessage
import org.bitcoins.node.serializers.messages.data.RawGetDataMessageSerializer
import org.bitcoins.node.messages.GetDataMessage
import scodec.bits.ByteVector

/**
  * Created by chris on 7/8/16.
  */
object GetDataMessage extends Factory[GetDataMessage] {
  private case class GetDataMessageImpl(
      inventoryCount: CompactSizeUInt,
      inventories: Seq[Inventory])
      extends GetDataMessage

  override def fromBytes(bytes: ByteVector): GetDataMessage = {
    RawGetDataMessageSerializer.read(bytes)
  }

  def apply(
      inventoryCount: CompactSizeUInt,
      inventories: Seq[Inventory]): GetDataMessage = {
    GetDataMessageImpl(inventoryCount, inventories)
  }

  def apply(inventories: Seq[Inventory]): GetDataMessage = {
    val inventoryCount = CompactSizeUInt(UInt64(inventories.length))
    GetDataMessage(inventoryCount, inventories)
  }

  def apply(inventory: Inventory): GetDataMessage =
    GetDataMessage(Seq(inventory))
}
