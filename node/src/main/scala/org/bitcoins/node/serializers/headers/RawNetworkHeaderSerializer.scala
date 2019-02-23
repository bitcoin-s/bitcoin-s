package org.bitcoins.node.serializers.headers

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.serializers.RawBitcoinSerializer
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.headers.NetworkHeader
import scodec.bits.ByteVector

/**
  * Created by chris on 5/31/16.
  * Reads and writes a message header on the peer-to-peer network
  * https://bitcoin.org/en/developer-reference#message-headers
  */
trait RawNetworkHeaderSerializer
    extends RawBitcoinSerializer[NetworkHeader]
    with BitcoinSLogger {

  /**
    * Transforms a sequence of bytes into a message header
    * @param bytes the byte representation for a MessageHeader on the peer-to-peer network
    * @return the native object for the MessageHeader
    */
  def read(bytes: ByteVector): NetworkHeader = {
    val network = bytes.take(4)
    //.trim removes the null characters appended to the command name
    val commandName = bytes.slice(4, 16).toArray.map(_.toChar).mkString.trim
    val payloadSize = UInt32(bytes.slice(16, 20).reverse)
    val checksum = bytes.slice(20, 24)
    NetworkHeader(network, commandName, payloadSize, checksum)
  }

  /**
    * Takes in a message header and serializes it to hex
    * @param messageHeader the message header to be serialized
    * @return the hexadecimal representation of the message header
    */
  def write(messageHeader: NetworkHeader): ByteVector = {
    val network = messageHeader.network
    val commandNameNoPadding = messageHeader.commandName.map(_.toByte)
    //command name needs to be 12 bytes in size, or 24 chars in hex
    val commandName = ByteVector(commandNameNoPadding).padRight(12)
    val checksum = messageHeader.checksum
    network ++
      commandName ++
      messageHeader.payloadSize.bytes.reverse ++
      checksum
  }

}

object RawNetworkHeaderSerializer extends RawNetworkHeaderSerializer
