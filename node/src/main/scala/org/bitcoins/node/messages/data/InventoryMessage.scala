package org.bitcoins.node.messages.data

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.util.Factory
import org.bitcoins.node.messages.InventoryMessage
import org.bitcoins.node.serializers.messages.data.RawInventoryMessageSerializer
import org.bitcoins.node.messages.InventoryMessage
import org.bitcoins.node.serializers.messages.data.RawInventoryMessageSerializer
import scodec.bits.ByteVector

/**
  * Created by chris on 6/1/16.
  * Creates an scala object that represents the inventory type on the p2p network
  * https://bitcoin.org/en/developer-reference#inv
  */
object InventoryMessage extends Factory[InventoryMessage] {

  private case class InventoryMessageImpl(
      inventoryCount: CompactSizeUInt,
      inventories: Seq[Inventory])
      extends InventoryMessage
  override def fromBytes(bytes: ByteVector): InventoryMessage =
    RawInventoryMessageSerializer.read(bytes)

  def apply(
      inventoryCount: CompactSizeUInt,
      inventories: Seq[Inventory]): InventoryMessage = {
    InventoryMessageImpl(inventoryCount, inventories)
  }

  def apply(inventories: Seq[Inventory]): InventoryMessage = {
    val count = CompactSizeUInt(UInt64(inventories.length))
    InventoryMessage(count, inventories)
  }
}
