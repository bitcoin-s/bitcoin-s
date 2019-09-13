package org.bitcoins.core.serializers.p2p.messages

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.p2p.GetCompactFilterCheckPointMessage
import org.bitcoins.core.serializers.RawBitcoinSerializer
import scodec.bits.ByteVector

/**
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#getcfcheckpt BIP157]]
  */
object RawGetCompactFilterCheckpointMessageSerializer
    extends RawBitcoinSerializer[GetCompactFilterCheckPointMessage] {

  def read(bytes: ByteVector): GetCompactFilterCheckPointMessage = {
    val filterType = FilterType.fromBytes(bytes.take(1))
    val stopHash = bytes.drop(1).take(32)
    GetCompactFilterCheckPointMessage(
      filterType = filterType,
      stopHash = DoubleSha256Digest.fromBytes(stopHash)
    )
  }

  def write(message: GetCompactFilterCheckPointMessage): ByteVector = {
    val filterType = message.filterType.bytes
    val stopHash = message.stopHash.bytes
    val bytes = filterType ++ stopHash
    bytes
  }
}
