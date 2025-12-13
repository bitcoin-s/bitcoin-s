package org.bitcoins.core.bip47

import org.bitcoins.core.crypto.ChainCode
import org.bitcoins.core.util.Base58
import org.bitcoins.crypto.{CryptoUtil, ECPublicKey, Factory, NetworkElement}
import scodec.bits.ByteVector

import scala.util.{Failure, Try}

/** Represents a BIP47 reusable payment code.
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0047.mediawiki BIP47]]
  */
sealed abstract class PaymentCode extends NetworkElement {

  def version: PaymentCodeVersion

  def pubKey: ECPublicKey

  def chainCode: ChainCode

  def features: Byte

  def payload: ByteVector = {
    val typeByte = version match {
      case PaymentCodeVersion.V1 => 0x01.toByte
      case PaymentCodeVersion.V2 => 0x02.toByte
    }
    ByteVector(typeByte,
               features) ++ pubKey.bytes ++ chainCode.bytes ++ ByteVector
      .fill(13)(0)
  }

  override def bytes: ByteVector =
    ByteVector(PaymentCode.VersionByte) ++ payload

  override def toString: String = {
    val checksum = CryptoUtil.doubleSHA256(bytes).bytes.take(4)
    Base58.encode(bytes ++ checksum)
  }

  def isValid: Boolean = {
    pubKey.bytes.size == 33 &&
    (pubKey.bytes.head == 0x02 || pubKey.bytes.head == 0x03)
    // chainCode size is already enforced by ChainCode constructor
  }
}

object PaymentCode extends Factory[PaymentCode] {
  val PayloadLength: Int = 80
  val VersionByte: Byte = 0x47

  private val PublicKeyYOffset: Int = 2
  private val PublicKeyXOffset: Int = 3
  private val ChainOffset: Int = 35
  private val PublicKeyXLen: Int = 32
  private val PublicKeyYLen: Int = 1
  private val ChainLen: Int = 32

  private case class PaymentCodeImpl(
      version: PaymentCodeVersion,
      pubKey: ECPublicKey,
      chainCode: ChainCode,
      features: Byte)
      extends PaymentCode

  def apply(pubKey: ECPublicKey, chainCode: ChainCode): PaymentCode = {
    apply(PaymentCodeVersion.V1, pubKey, chainCode, 0x00.toByte)
  }

  def apply(
      version: PaymentCodeVersion,
      pubKey: ECPublicKey,
      chainCode: ChainCode,
      features: Byte): PaymentCode = {
    require(pubKey.isCompressed,
            "Payment code public key must be compressed (33 bytes)")
    PaymentCodeImpl(version, pubKey, chainCode, features)
  }

  def fromBase58(str: String): Try[PaymentCode] = {
    Base58.decodeCheck(str).flatMap { decoded =>
      if (decoded.isEmpty || decoded.head != VersionByte) {
        Failure(
          new IllegalArgumentException(
            s"Invalid payment code version byte, expected 0x47"))
      } else {
        Try(fromBytes(decoded))
      }
    }
  }

  override def fromBytes(bytes: ByteVector): PaymentCode = {
    require(
      bytes.size >= PayloadLength + 1,
      s"Payment code must be at least ${PayloadLength + 1} bytes, got ${bytes.size}")

    val payload =
      if (bytes.head == VersionByte) {
        bytes.drop(1).take(PayloadLength)
      } else {
        bytes.take(PayloadLength)
      }

    val typeByte = payload(0)
    val version = typeByte match {
      case 0x01 => PaymentCodeVersion.V1
      case 0x02 => PaymentCodeVersion.V2
      case other =>
        throw new IllegalArgumentException(s"Unknown payment code type: $other")
    }

    val features = payload(1)
    val pubKeyBytes =
      payload.slice(PublicKeyYOffset,
                    PublicKeyYOffset + PublicKeyYLen + PublicKeyXLen)
    val chainCodeBytes = payload.slice(ChainOffset, ChainOffset + ChainLen)

    require(
      pubKeyBytes.head == 0x02 || pubKeyBytes.head == 0x03,
      s"Invalid public key prefix: ${pubKeyBytes.head}"
    )

    val pubKey = ECPublicKey(pubKeyBytes)
    val chainCode = ChainCode(chainCodeBytes)

    PaymentCodeImpl(version, pubKey, chainCode, features)
  }

  /** Generates a mask for blinding a payment code in a notification
    * transaction.
    */
  def getMask(secretPoint: ByteVector, outpoint: ByteVector): ByteVector = {
    CryptoUtil.hmac512(outpoint, secretPoint)
  }

  /** Blinds or unblinds a payment code payload using XOR masking. */
  def blind(payload: ByteVector, mask: ByteVector): ByteVector = {
    require(payload.size == PayloadLength,
            s"Payload must be $PayloadLength bytes")
    require(mask.size == 64, "Mask must be 64 bytes")

    val pubKeyX =
      payload.slice(PublicKeyXOffset, PublicKeyXOffset + PublicKeyXLen)
    val chain = payload.slice(ChainOffset, ChainOffset + ChainLen)

    val maskPubKey = mask.take(PublicKeyXLen)
    val maskChain = mask.slice(PublicKeyXLen, PublicKeyXLen + ChainLen)

    val blindedPubKeyX = xor(pubKeyX, maskPubKey)
    val blindedChain = xor(chain, maskChain)

    payload.take(PublicKeyXOffset) ++
      blindedPubKeyX ++
      payload.slice(PublicKeyXOffset + PublicKeyXLen, ChainOffset) ++
      blindedChain ++
      payload.drop(ChainOffset + ChainLen)
  }

  private def xor(a: ByteVector, b: ByteVector): ByteVector = {
    require(a.size == b.size,
            s"XOR operands must have same size: ${a.size} vs ${b.size}")
    a.xor(b)
  }
}
