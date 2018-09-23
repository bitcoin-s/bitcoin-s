package org.bitcoins.core.protocol.ln

import org.bitcoins.core.crypto.ECDigitalSignature
import org.bitcoins.core.number.{ UInt5, UInt8 }
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Bech32
import scodec.bits.ByteVector

sealed abstract class LnInvoiceSignature {
  require(version.toInt >= 0 && version.toInt <= 3, s"signature recovery byte must be 0,1,2,3, got ${version.toInt}")

  def signature: ECDigitalSignature

  def version: UInt8

  def data: Vector[UInt5] = {
    val bytes = signature.toRawRS ++ version.bytes
    Bech32.from8bitTo5bit(bytes)
  }
}

object LnInvoiceSignature {
  private case class LnInvoiceSignatureImpl(
    version: UInt8,
    signature: ECDigitalSignature) extends LnInvoiceSignature

  def apply(version: UInt8, signature: ECDigitalSignature): LnInvoiceSignature = {
    LnInvoiceSignatureImpl(version, signature)
  }

  def fromBytes(bytes: ByteVector): LnInvoiceSignature = {
    val sigBytes = bytes.take(64)
    val version = UInt8(bytes(64))

    val signature = ECDigitalSignature.fromRS(sigBytes)

    LnInvoiceSignature.apply(
      version = version,
      signature = signature)
  }

  def fromU5s(u5s: Vector[UInt5]): LnInvoiceSignature = {
    val u8s = Bech32.from5bitTo8bit(u5s)
    val bytes = UInt8.toBytes(u8s)
    fromBytes(bytes)
  }
}
