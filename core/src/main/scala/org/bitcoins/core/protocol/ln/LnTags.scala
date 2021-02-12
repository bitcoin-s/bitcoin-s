package org.bitcoins.core.protocol.ln

import java.nio.charset.Charset

import org.bitcoins.core.config.{MainNet, NetworkParameters}
import org.bitcoins.core.number.{UInt32, UInt5, UInt8}
import org.bitcoins.core.protocol._
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.core.protocol.ln.routing.LnRoute
import org.bitcoins.core.protocol.ln.util.LnUtil
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WSHWitnessSPKV0}
import org.bitcoins.core.util.{Bech32, SeqWrapper}
import org.bitcoins.crypto.{CryptoUtil, Sha256Digest, Sha256Hash160Digest}
import scodec.bits.ByteVector

import scala.annotation.tailrec

/** One of the tagged fields on a Lightning Network invoice
  * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields]]
  */
sealed trait LnTag {

  def prefix: LnTagPrefix

  def prefixUInt5: UInt5 = {
    val char = Bech32.charset.indexOf(prefix.value)
    UInt5(char.toByte)
  }

  /** The payload for the tag without any meta information encoded with it */
  def encoded: Vector[UInt5]

  def data: Vector[UInt5] = {
    val b = Vector.newBuilder[UInt5]
    val len = LnUtil.createDataLength(encoded.toList)

    b.+=(prefixUInt5)

    len.foreach(u5 => b.+=(u5))

    encoded.foreach(u5 => b.+=(u5))

    b.result()
  }

  override def toString: String = {

    val dataBech32 = Bech32.encode5bitToString(data)

    dataBech32

  }
}

/** All of the different invoice tags that are currently defined
  * Refer to BOLT11 for a full list
  * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields]]
  */
object LnTag {

  sealed abstract class FallbackAddressV {
    val u8: UInt8
  }

  /** Fallback address versions */
  object FallbackAddressV {

    case object P2PKH extends FallbackAddressV {
      val u8: UInt8 = UInt8(17)
    }

    case object P2SH extends FallbackAddressV {
      val u8: UInt8 = UInt8(18)
    }

    case object WitSPK extends FallbackAddressV {
      val u8 = UInt8.zero
    }

    private def witnessFromU8(bytes: ByteVector, np: NetworkParameters) = {
      val witSPK = bytes.size match {
        case 32 =>
          val hash = Sha256Digest.fromBytes(bytes)
          P2WSHWitnessSPKV0.fromHash(hash)
        case 20 =>
          val hash = Sha256Hash160Digest.fromBytes(bytes)
          P2WPKHWitnessSPKV0.fromHash(hash)
        case _: Long =>
          throw new IllegalArgumentException(
            s"Can only create witness spk out of a 32 byte or 20 byte hash, got ${bytes.length}")
      }
      Bech32Address(witSPK, np)
    }

    def fromU8(
        version: UInt8,
        bytes: ByteVector,
        np: NetworkParameters): FallbackAddressTag = {
      val address: Address = version match {
        case P2PKH.u8 =>
          val hash = Sha256Hash160Digest(bytes)
          P2PKHAddress(hash, np)
        case P2SH.u8 =>
          val hash = Sha256Hash160Digest(bytes)
          P2SHAddress(hash, np)
        case WitSPK.u8 => witnessFromU8(bytes, np)
        case _: UInt8 =>
          throw new IllegalArgumentException(
            s"Illegal version to create a fallback address from, got $version")
      }
      LnTag.FallbackAddressTag(address)
    }
  }

  private def u32ToU5(u32: UInt32): Vector[UInt5] = {
    val encoded = LnUtil.encodeNumber(u32.toLong)
    encoded.toVector
  }

  case class PaymentHashTag(hash: Sha256Digest) extends LnTag {

    override val prefix: LnTagPrefix = LnTagPrefix.PaymentHash

    override val encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(hash.bytes)
    }
  }

  case class SecretTag(secret: PaymentSecret) extends LnTag {

    override val prefix: LnTagPrefix = LnTagPrefix.Secret

    override val encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(secret.bytes)
    }
  }

  case class DescriptionTag(string: String) extends LnTag {
    override val prefix: LnTagPrefix = LnTagPrefix.Description

    def descBytes: ByteVector = {
      ByteVector(string.getBytes("UTF-8"))
    }

    def descriptionHashTag: LnTag.DescriptionHashTag = {
      val hash = CryptoUtil.sha256(descBytes)
      LnTag.DescriptionHashTag(hash)
    }

    override val encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(descBytes)
    }

  }

  case class NodeIdTag(nodeId: NodeId) extends LnTag {

    override val prefix: LnTagPrefix = LnTagPrefix.NodeId

    override val encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(nodeId.bytes)
    }
  }

  case class DescriptionHashTag(hash: Sha256Digest) extends LnTag {
    override val prefix: LnTagPrefix = LnTagPrefix.DescriptionHash

    override val encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(hash.bytes)
    }
  }

  /** The amount in seconds until this payment request expires */
  case class ExpiryTimeTag(u32: UInt32) extends LnTag {
    override val prefix: LnTagPrefix = LnTagPrefix.ExpiryTime

    override val encoded: Vector[UInt5] = {
      LnUtil.encodeNumber(u32.toLong).toVector
    }
  }

  /** `min_final_ctlv_expiry` is the minimum difference between
    * HTLC CLTV timeout and the current block height, for the
    * terminal case (C). This is denominated in blocks.
    * [[https://github.com/lightningnetwork/lightning-rfc/blob/master/02-peer-protocol.md#cltv_expiry_delta-selection]]
    */
  case class MinFinalCltvExpiry(u32: UInt32) extends LnTag {
    override val prefix: LnTagPrefix = LnTagPrefix.CltvExpiry

    override val encoded: Vector[UInt5] = {
      u32ToU5(u32)
    }

  }

  case class FallbackAddressTag(address: Address) extends LnTag {

    /** The version of the fallback address is indicated here in BOLT11 */
    def version: UInt8 = {
      address match {
        case _: P2PKHAddress => FallbackAddressV.P2PKH.u8
        case _: P2SHAddress  => FallbackAddressV.P2SH.u8
        case bech32: Bech32Address =>
          UInt8(bech32.scriptPubKey.witnessVersion.version.toInt)
      }
    }

    override val prefix: LnTagPrefix = LnTagPrefix.FallbackAddress

    override val encoded: Vector[UInt5] = {
      val b = address.hash.bytes
      val u5s = version.toUInt5 +: Bech32.from8bitTo5bit(b)
      u5s
    }
  }

  case class RoutingInfo(routes: Vector[LnRoute])
      extends SeqWrapper[LnRoute]
      with LnTag {
    override protected val wrapped: Vector[LnRoute] = routes

    override val prefix: LnTagPrefix = LnTagPrefix.RoutingInfo

    override val encoded: Vector[UInt5] = {
      if (routes.isEmpty) {
        Vector.empty
      } else {
        val serializedRoutes: ByteVector = {
          routes.foldLeft(ByteVector.empty)(_ ++ _.bytes)
        }

        val u5s = Bech32.from8bitTo5bit(serializedRoutes)
        u5s
      }
    }
  }

  object RoutingInfo {

    def fromU5s(u5s: Vector[UInt5]): RoutingInfo = {

      @tailrec
      def loop(
          remaining: ByteVector,
          accum: Vector[LnRoute]): Vector[LnRoute] = {
        if (remaining.isEmpty) {
          accum
        } else {
          val route = LnRoute.fromBytes(remaining)
          val newRemaining = remaining.slice(route.byteSize, remaining.size)
          loop(newRemaining, accum.:+(route))
        }
      }

      val bytes = UInt8.toBytes(Bech32.from5bitTo8bit(u5s))
      val vecRoutes: Vector[LnRoute] = loop(bytes, Vector.empty)

      LnTag.RoutingInfo(vecRoutes)

    }
  }

  def fromLnTagPrefix(prefix: LnTagPrefix, payload: Vector[UInt5]): LnTag = {

    val u8s = Bech32.from5bitTo8bit(payload)
    val bytes = UInt8.toBytes(u8s)

    val tag: LnTag = prefix match {
      case LnTagPrefix.PaymentHash =>
        val hash = Sha256Digest.fromBytes(bytes)
        LnTag.PaymentHashTag(hash)

      case LnTagPrefix.Secret =>
        LnTag.SecretTag(PaymentSecret.fromBytes(bytes))

      case LnTagPrefix.Description =>
        val description = new String(bytes.toArray, Charset.forName("UTF-8"))
        LnTag.DescriptionTag(description)

      case LnTagPrefix.DescriptionHash =>
        val hash = Sha256Digest.fromBytes(bytes)
        LnTag.DescriptionHashTag(hash)

      case LnTagPrefix.NodeId =>
        val nodeId = NodeId.fromBytes(bytes)

        LnTag.NodeIdTag(nodeId)

      case LnTagPrefix.ExpiryTime =>
        val decoded = LnUtil.decodeNumber(payload.toList)
        val u32 = UInt32(decoded)
        LnTag.ExpiryTimeTag(u32)

      case LnTagPrefix.CltvExpiry =>
        val decoded = LnUtil.decodeNumber(payload.toList)
        val u32 = UInt32(decoded)
        LnTag.MinFinalCltvExpiry(u32)

      case LnTagPrefix.FallbackAddress =>
        val version = payload.head.toUInt8
        val noVersion = payload.tail
        val noVersionBytes =
          UInt8.toBytes(Bech32.from5bitTo8bit(noVersion))
        FallbackAddressV.fromU8(version, noVersionBytes, MainNet)

      case LnTagPrefix.RoutingInfo =>
        RoutingInfo.fromU5s(payload)

      case LnTagPrefix.Features =>
        LnTag.FeaturesTag(bytes)

      case prefix: LnTagPrefix.Unknown =>
        LnTag.UnknownTag(prefix, payload)
    }

    tag
  }

  case class FeaturesTag(features: ByteVector) extends LnTag {
    override def prefix: LnTagPrefix = LnTagPrefix.Features

    /** The payload for the tag without any meta information encoded with it */
    override def encoded: Vector[UInt5] = {
      Bech32.from8bitTo5bit(features)
    }
  }

  case class UnknownTag(prefix: LnTagPrefix, encoded: Vector[UInt5])
      extends LnTag
}
