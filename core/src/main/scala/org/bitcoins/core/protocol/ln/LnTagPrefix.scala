package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.util.Bech32

sealed abstract class LnTagPrefix {
  val value: Char

  override def toString: String = value.toString
}

/**
  * This defines the necessary Lightning Network Tag Prefix's, as specified in BOLT-11
  * Please see: https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields
  */
object LnTagPrefix {

  case class Unknown(value: Char) extends LnTagPrefix

  case object PaymentHash extends LnTagPrefix {
    override val value: Char = 'p'
  }
  case object Secret extends LnTagPrefix {
    override val value: Char = 's'
  }
  case object Description extends LnTagPrefix {
    override val value: Char = 'd'
  }

  /** The nodeId of the node paying the invoice */
  case object NodeId extends LnTagPrefix {
    override val value: Char = 'n'
  }

  case object DescriptionHash extends LnTagPrefix {
    override val value: Char = 'h'
  }

  case object ExpiryTime extends LnTagPrefix {
    override val value: Char = 'x'
  }

  case object CltvExpiry extends LnTagPrefix {
    override val value: Char = 'c'
  }

  case object FallbackAddress extends LnTagPrefix {
    override val value: Char = 'f'
  }

  case object RoutingInfo extends LnTagPrefix {
    override val value: Char = 'r'
  }

  case object Features extends LnTagPrefix {
    override val value: Char = '9'
  }

  private lazy val allKnown: Map[Char, LnTagPrefix] =
    List(PaymentHash,
         Description,
         NodeId,
         DescriptionHash,
         ExpiryTime,
         CltvExpiry,
         FallbackAddress,
         RoutingInfo,
         Features,
         Secret)
      .map(prefix => prefix.value -> prefix)
      .toMap

  def fromString(str: String): Option[LnTagPrefix] = {
    if (str.length == 1) {
      Option(fromChar(str.head))
    } else {
      None
    }
  }

  def fromChar(char: Char): LnTagPrefix = {
    allKnown.getOrElse(char, Unknown(char))
  }

  private lazy val prefixUInt5: Map[UInt5, LnTagPrefix] = {
    val all = Bech32.charset
      .map(value => (value, Unknown(value)))
      .toMap

    (all ++ allKnown).map {
      case (_, prefix) =>
        val index = Bech32.charset.indexOf(prefix.value)
        (UInt5(index), prefix)
    }
  }

  def fromUInt5(u5: UInt5): Option[LnTagPrefix] = {
    val p = prefixUInt5.get(u5)
    p
  }
}
