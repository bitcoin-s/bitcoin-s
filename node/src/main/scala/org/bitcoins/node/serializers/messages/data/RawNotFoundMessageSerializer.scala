package org.bitcoins.node.serializers.messages.data

import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.node.messages.NotFoundMessage
import org.bitcoins.node.messages.data.{InventoryMessage, NotFoundMessage}
import org.bitcoins.node.messages.NotFoundMessage
import org.bitcoins.node.messages.data.{InventoryMessage, NotFoundMessage}
import scodec.bits.ByteVector

/**
  * Created by chris on 6/2/16.
  * Responsible for the serialization and deserialization of a NotFound message on the p2p network
  * https://bitcoin.org/en/developer-reference#notfound
  */
trait RawNotFoundMessageSerializer
    extends RawBitcoinSerializer[NotFoundMessage] {

  override def read(bytes: ByteVector): NotFoundMessage = {
    //this seems funky, but according to the documentation inventory messages
    //and NotFoundMessages have the same structure, therefore we can piggy back
    //off of the serializer used by InventoryMessage
    val inventoryMessage = InventoryMessage(bytes)
    NotFoundMessage(inventoryMessage.inventoryCount,
                    inventoryMessage.inventories)

  }

  override def write(notFoundMessage: NotFoundMessage): ByteVector = {
    //Since InventoryMessages and NotFoundMessages have the same format
    //we can just create an inventory message then piggy back off of the
    //serializer used by inventory message
    val inventoryMessage = InventoryMessage(notFoundMessage.inventoryCount,
                                            notFoundMessage.inventories)
    inventoryMessage.bytes
  }

}

object RawNotFoundMessageSerializer extends RawNotFoundMessageSerializer
