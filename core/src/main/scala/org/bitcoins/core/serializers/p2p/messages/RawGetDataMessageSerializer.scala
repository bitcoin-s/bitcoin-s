package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p.{GetDataMessage, InventoryMessage}
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

/** @see https://bitcoin.org/en/developer-reference#getdata
  */
trait RawGetDataMessageSerializer extends RawBitcoinSerializer[GetDataMessage] {
  //InventoryMessages & GetDataMessages have the same structure and are serialized the same
  //so we can piggy back off of the serialilzers for InventoryMessages

  def read(bytes: ByteVector): GetDataMessage = {
    val inv = InventoryMessage(bytes)
    GetDataMessage(inv.inventoryCount, inv.inventories)
  }

  def write(getDataMessage: GetDataMessage): ByteVector = {
    val inv = InventoryMessage(getDataMessage.inventoryCount,
                               getDataMessage.inventories)
    inv.bytes
  }
}

object RawGetDataMessageSerializer extends RawGetDataMessageSerializer
