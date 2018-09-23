package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.protocol.ln.LnInvoiceTag.PaymentHashTag
import org.bitcoins.core.util.Bech32
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * An aggregation of all the individual tagged fields in a [[org.bitcoins.core.protocol.ln.LnInvoice]]
 */
sealed abstract class LnInvoiceTaggedFields {

  def paymentHash: LnInvoiceTag.PaymentHashTag

  def description: Option[LnInvoiceTag.DescriptionTag]

  def nodeId: Option[LnInvoiceTag.NodeIdTag]

  def descriptionHash: Option[LnInvoiceTag.DescriptionHashTag]

  def expiryTime: Option[LnInvoiceTag.ExpiryTimeTag]

  def cltvExpiry: Option[LnInvoiceTag.MinFinalCltvExpiry]

  def fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag]

  def routingInfo: Option[LnInvoiceTag.RoutingInfo]

  def data: Vector[UInt5] = {
    paymentHash.data ++
      description.map(_.data).getOrElse(Vector.empty) ++
      nodeId.map(_.data).getOrElse(Vector.empty) ++
      descriptionHash.map(_.data).getOrElse(Vector.empty) ++
      expiryTime.map(_.data).getOrElse(Vector.empty) ++
      cltvExpiry.map(_.data).getOrElse(Vector.empty) ++
      fallbackAddress.map(_.data).getOrElse(Vector.empty) ++
      routingInfo.map(_.data).getOrElse(Vector.empty)
  }

  override def toString: String = {
    val b = new mutable.StringBuilder()

    val string = Bech32.encode5bitToString(data)
    b.append(string)

    b.toString()
  }
}

object LnInvoiceTaggedFields extends {
  private case class InvoiceTagImpl(
    paymentHash: LnInvoiceTag.PaymentHashTag,
    description: Option[LnInvoiceTag.DescriptionTag],
    nodeId: Option[LnInvoiceTag.NodeIdTag],
    descriptionHash: Option[LnInvoiceTag.DescriptionHashTag],
    expiryTime: Option[LnInvoiceTag.ExpiryTimeTag],
    cltvExpiry: Option[LnInvoiceTag.MinFinalCltvExpiry],
    fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag],
    routingInfo: Option[LnInvoiceTag.RoutingInfo]) extends LnInvoiceTaggedFields {
    require(
      (description.nonEmpty && description.get.string.length < 640) ||
        descriptionHash.nonEmpty,
      "You must supply either a description hash, or a literal description that is 640 characters or less to create an invoice.")
  }

  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)

  /**
   * According to BOLT11 these are the required fields in a LnInvoice
   * You need to provide a payment hash and either a description,
   * or the hash of the description
   */
  def apply(
    paymentHashTag: PaymentHashTag,
    descriptionOrHash: Either[LnInvoiceTag.DescriptionTag, LnInvoiceTag.DescriptionHashTag]): LnInvoiceTaggedFields = {

    LnInvoiceTaggedFields.apply(paymentHashTag, descriptionOrHash)
  }

  def apply(
    paymentHash: LnInvoiceTag.PaymentHashTag,
    descriptionOrHash: Either[LnInvoiceTag.DescriptionTag, LnInvoiceTag.DescriptionHashTag],
    nodeId: Option[LnInvoiceTag.NodeIdTag] = None,
    expiryTime: Option[LnInvoiceTag.ExpiryTimeTag] = None,
    cltvExpiry: Option[LnInvoiceTag.MinFinalCltvExpiry] = None,
    fallbackAddress: Option[LnInvoiceTag.FallbackAddressTag] = None,
    routingInfo: Option[LnInvoiceTag.RoutingInfo] = None): LnInvoiceTaggedFields = {

    if (descriptionOrHash.isLeft) {
      InvoiceTagImpl(
        paymentHash = paymentHash,
        description = descriptionOrHash.left.toOption,
        nodeId = nodeId,
        descriptionHash = None,
        expiryTime = expiryTime,
        cltvExpiry = cltvExpiry,
        fallbackAddress = fallbackAddress,
        routingInfo = routingInfo)
    } else {

      InvoiceTagImpl(
        paymentHash = paymentHash,
        description = None,
        nodeId = nodeId,
        descriptionHash = descriptionOrHash.right.toOption,
        expiryTime = expiryTime,
        cltvExpiry = cltvExpiry,
        fallbackAddress = fallbackAddress,
        routingInfo = routingInfo)
    }

  }

  def fromUInt5s(u5s: Vector[UInt5]): LnInvoiceTaggedFields = {
    @tailrec
    def loop(remaining: List[UInt5], fields: Vector[LnInvoiceTag]): Vector[LnInvoiceTag] = {
      remaining match {
        case h :: h1 :: h2 :: t =>

          //first u5 is the prefix
          val prefix = LnTagPrefix.fromUInt5(h)

          //next two 5 bit increments are data_length
          val dataLengthU5s = Vector(h1, h2)

          val dataLength = LnInvoiceTag.decodeNumber(dataLengthU5s)

          //t is the actual possible payload
          val payload: Vector[UInt5] = t.take(dataLength.toInt).toVector

          val tag = LnInvoiceTag.fromLnTagPrefix(prefix.get, payload)

          val newRemaining = t.slice(payload.size, t.size)

          loop(newRemaining, fields.:+(tag))

        case _ :: _ | _ :: _ :: _ =>
          throw new IllegalArgumentException("Failed to parse LnInvoiceTaggedFields, needs 15bits of meta data to be able to parse")
        case Nil =>
          fields
      }
    }

    val tags = loop(u5s.toList, Vector.empty)

    val paymentHashTag = tags.find(_.isInstanceOf[LnInvoiceTag.PaymentHashTag])
      .get.asInstanceOf[LnInvoiceTag.PaymentHashTag]

    val description = tags.find(_.isInstanceOf[LnInvoiceTag.DescriptionTag])
      .map(_.asInstanceOf[LnInvoiceTag.DescriptionTag])

    val descriptionHash = tags.find(_.isInstanceOf[LnInvoiceTag.DescriptionHashTag])
      .map(_.asInstanceOf[LnInvoiceTag.DescriptionHashTag])

    val nodeId = tags.find(_.isInstanceOf[LnInvoiceTag.NodeIdTag])
      .map(_.asInstanceOf[LnInvoiceTag.NodeIdTag])

    val expiryTime = tags.find(_.isInstanceOf[LnInvoiceTag.ExpiryTimeTag])
      .map(_.asInstanceOf[LnInvoiceTag.ExpiryTimeTag])

    val cltvExpiry = tags.find(_.isInstanceOf[LnInvoiceTag.MinFinalCltvExpiry])
      .map(_.asInstanceOf[LnInvoiceTag.MinFinalCltvExpiry])

    val fallbackAddress = tags.find(_.isInstanceOf[LnInvoiceTag.FallbackAddressTag])
      .map(_.asInstanceOf[LnInvoiceTag.FallbackAddressTag])

    val routingInfo = tags.find(_.isInstanceOf[LnInvoiceTag.RoutingInfo])
      .map(_.asInstanceOf[LnInvoiceTag.RoutingInfo])

    val d: Either[LnInvoiceTag.DescriptionTag, LnInvoiceTag.DescriptionHashTag] = {
      if (description.isDefined && descriptionHash.isDefined) {
        throw new IllegalArgumentException(s"Cannot have both description and description hash")
      } else if (description.isEmpty && descriptionHash.isEmpty) {
        throw new IllegalArgumentException(s"One of description / description hash fields must be defind")
      } else if (description.isDefined) {
        Left(description.get)
      } else {
        Right(descriptionHash.get)
      }
    }

    LnInvoiceTaggedFields(
      paymentHash = paymentHashTag,
      descriptionOrHash = d,
      nodeId = nodeId,
      expiryTime = expiryTime,
      cltvExpiry = cltvExpiry,
      fallbackAddress = fallbackAddress,
      routingInfo = routingInfo)

  }
}