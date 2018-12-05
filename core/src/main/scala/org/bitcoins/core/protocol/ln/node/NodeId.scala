package org.bitcoins.core.protocol.ln.node

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

/**
 * `NodeId` is simply a wrapper for
  * [[org.bitcoins.core.crypto.ECPublicKey ECPublicKey]].
 * This public key needs to be a
 * 33 byte compressed secp256k1 public key.
 */
case class NodeId(pubKey: ECPublicKey) extends NetworkElement {
  require(pubKey.isCompressed, s"Cannot create a nodeId from a public key that was not compressed ${pubKey.hex}")

  override def toString: String = pubKey.hex

  override def bytes: ByteVector = pubKey.bytes
}

object NodeId extends Factory[NodeId] {

  def fromPubKey(pubKey: ECPublicKey): NodeId = {
    NodeId(pubKey)
  }

  override def fromBytes(bytes: ByteVector): NodeId = {
    val pubKey = ECPublicKey.fromBytes(bytes)
    fromPubKey(pubKey)
  }
}
