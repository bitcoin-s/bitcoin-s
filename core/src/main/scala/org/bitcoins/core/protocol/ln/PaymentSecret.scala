package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.{ECPrivateKey, Sha256Digest}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.{CryptoUtil, Factory}
import scodec.bits.ByteVector

final case class PaymentSecret(bytes: ByteVector) extends NetworkElement {
  require(bytes.size == 32,
          s"Payment secret must be 32 bytes in size, got: " + bytes.length)

  lazy val hash: Sha256Digest = CryptoUtil.sha256(bytes)
}

object PaymentSecret extends Factory[PaymentSecret] {

  override def fromBytes(bytes: ByteVector): PaymentSecret = {
    new PaymentSecret(bytes)
  }

  def random: PaymentSecret = fromBytes(ECPrivateKey.freshPrivateKey.bytes)

}
