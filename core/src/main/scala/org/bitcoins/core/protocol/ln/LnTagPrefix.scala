package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.util.Bech32
import org.slf4j.LoggerFactory

sealed abstract class LnTagPrefix {
  def value: Char

  override def toString: String = value.toString
}

/**
 * This defines the necessary Lightning Network Tag Prefix's, as specified in BOLT-11
 * Please see: https://github.com/lightningnetwork/lightning-rfc/blob/master/11-payment-encoding.md#tagged-fields
 */
object LnTagPrefix {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  case object PaymentHash extends LnTagPrefix {
    override def value: Char = 'p'
  }
  case object Description extends LnTagPrefix {
    override def value: Char = 'd'
  }

  /** The nodeId of the node paying the invoice */
  case object NodeId extends LnTagPrefix {
    override def value: Char = 'n'
  }

  case object DescriptionHash extends LnTagPrefix {
    override def value: Char = 'h'
  }

  case object ExpiryTime extends LnTagPrefix {
    override def value: Char = 'x'
  }

  case object CltvExpiry extends LnTagPrefix {
    override def value: Char = 'c'
  }

  case object FallbackAddress extends LnTagPrefix {
    override def value: Char = 'f'
  }

  case object RoutingInfo extends LnTagPrefix {
    override def value: Char = 'r'
  }

  private val all: List[LnTagPrefix] = List(
    PaymentHash, Description, NodeId,
    DescriptionHash, ExpiryTime, CltvExpiry,
    FallbackAddress, RoutingInfo)

  def fromString(str: String): Option[LnTagPrefix] = {
    all.find(_.value == str)
  }

  private lazy val prefixUInt5: Map[UInt5, LnTagPrefix] = {
    all.map { a =>
      val index = Bech32.charset.indexOf(a.value)
      (UInt5(index), a)
    }.toMap
  }

  def fromUInt5(u5: UInt5): Option[LnTagPrefix] = {
    prefixUInt5.get(u5)
  }
}