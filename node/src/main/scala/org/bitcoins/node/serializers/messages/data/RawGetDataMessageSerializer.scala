package org.bitcoins.node.serializers.messages.data

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.GetDataMessage
import org.bitcoins.node.messages.data.{GetDataMessage, InventoryMessage}
import org.bitcoins.node.messages.data.{GetDataMessage, InventoryMessage}
import scodec.bits.ByteVector

/**
  * Created by chris on 7/8/16.
  * https://bitcoin.org/en/developer-reference#getdata
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
