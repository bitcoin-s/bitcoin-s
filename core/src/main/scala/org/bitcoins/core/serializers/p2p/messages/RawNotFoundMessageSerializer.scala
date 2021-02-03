package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p.{InventoryMessage, NotFoundMessage}
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

/** Responsible for the serialization and deserialization of a NotFound message on the p2p network
  * @see https://bitcoin.org/en/developer-reference#notfound
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
