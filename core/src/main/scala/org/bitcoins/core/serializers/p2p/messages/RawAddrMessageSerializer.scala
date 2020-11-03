package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.p2p._
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.serializers.p2p._
import org.bitcoins.core.serializers.{RawBitcoinSerializer, RawSerializerHelper}
import scodec.bits.ByteVector

import scala.annotation.tailrec

/**
  * Responsible for the serialization and deserialization of AddrMessages
  * @see https://bitcoin.org/en/developer-reference#addr
  */
trait RawAddrMessageSerializer extends RawBitcoinSerializer[AddrMessage] {

  override def read(bytes: ByteVector): AddrMessage = {
    val ipCount = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val ipAddressBytes = bytes.slice(ipCount.byteSize.toInt, bytes.size)
    val (networkIpAddresses, _) =
      parseNetworkIpAddresses(ipCount, ipAddressBytes)
    AddrMessage(ipCount, networkIpAddresses)
  }

  override def write(addrMessage: AddrMessage): ByteVector = {
    addrMessage.ipCount.bytes ++
      RawSerializerHelper.write(ts = addrMessage.addresses,
                                serializer =
                                  RawNetworkIpAddressSerializer.write)
  }

  /**
    * Parses ip addresses inside of an AddrMessage
    * @param ipCount the number of ip addresses we need to parse from the AddrMessage
    * @param bytes the bytes from which we need to parse the ip addresses
    * @return the parsed ip addresses and the remaining bytes
    */
  private def parseNetworkIpAddresses(
      ipCount: CompactSizeUInt,
      bytes: ByteVector): (Seq[NetworkIpAddress], ByteVector) = {
    @tailrec
    def loop(
        remainingAddresses: BigInt,
        remainingBytes: ByteVector,
        accum: List[NetworkIpAddress]): (Seq[NetworkIpAddress], ByteVector) = {
      if (remainingAddresses <= 0) (accum.reverse, remainingBytes)
      else {
        val networkIpAddress =
          RawNetworkIpAddressSerializer.read(remainingBytes)
        val newRemainingBytes =
          remainingBytes.slice(networkIpAddress.byteSize, remainingBytes.size)
        loop(remainingAddresses - 1,
             newRemainingBytes,
             networkIpAddress :: accum)
      }
    }
    loop(ipCount.num.toInt, bytes, List())
  }
}

object RawAddrMessageSerializer extends RawAddrMessageSerializer
