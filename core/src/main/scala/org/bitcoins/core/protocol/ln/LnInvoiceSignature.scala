package org.bitcoins.core.protocol.ln

import java.math.BigInteger

import org.bitcoins.core.number.{UInt5, UInt8}
import org.bitcoins.core.util.Bech32
import org.bitcoins.crypto.{ECDigitalSignature, Factory, NetworkElement}
import scodec.bits.ByteVector

/**
  * 520 bit digital signature that signs the [[org.bitcoins.core.protocol.ln.LnInvoice LnInvoice]];
  * See
  * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#data-part BOLT11]]
  * for more info.
  */
sealed abstract class LnInvoiceSignature extends NetworkElement {
  require(recoverId.toInt >= 0 && recoverId.toInt <= 3,
          s"signature recovery byte must be 0,1,2,3, got ${recoverId.toInt}")

  require(
    bytes.length == 65,
    s"LnInvoiceSignatures MUST be 65 bytes in length, got ${bytes.length}")
  def signature: ECDigitalSignature

  def recoverId: UInt8

  def data: Vector[UInt5] = {
    val bytes = signature.toRawRS ++ recoverId.bytes
    Bech32.from8bitTo5bit(bytes)
  }

  override def bytes: ByteVector = {
    signature.toRawRS ++ recoverId.bytes
  }
}

object LnInvoiceSignature extends Factory[LnInvoiceSignature] {

  private case class LnInvoiceSignatureImpl(
      recoverId: UInt8,
      signature: ECDigitalSignature)
      extends LnInvoiceSignature

  def apply(
      recoverId: UInt8,
      signature: ECDigitalSignature): LnInvoiceSignature = {
    LnInvoiceSignatureImpl(recoverId, signature)
  }

  def fromBytes(bytes: ByteVector): LnInvoiceSignature = {
    val sigBytes = bytes.take(64)
    val recoverId = UInt8(bytes(64))

    val signature = ECDigitalSignature.fromRS(sigBytes)

    LnInvoiceSignature.apply(recoverId = recoverId, signature = signature)
  }

  def fromRS(
      r: BigInteger,
      s: BigInteger,
      recovId: UInt8): LnInvoiceSignature = {
    val sig = ECDigitalSignature.fromRS(r, s)
    LnInvoiceSignature(recovId, sig)
  }

  def fromU5s(u5s: Vector[UInt5]): LnInvoiceSignature = {
    val u8s = Bech32.from5bitTo8bit(u5s)
    val bytes = UInt8.toBytes(u8s)
    fromBytes(bytes)
  }
}
