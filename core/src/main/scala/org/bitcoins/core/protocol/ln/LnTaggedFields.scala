package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.UInt5
import org.bitcoins.core.protocol.ln.LnTag.PaymentHashTag
import org.bitcoins.core.protocol.ln.util.LnUtil
import org.bitcoins.core.util.Bech32
import org.slf4j.LoggerFactory

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * An aggregation of all the individual tagged fields in a [[org.bitcoins.core.protocol.ln.LnInvoice]]
 */
sealed abstract class LnTaggedFields {

  def paymentHash: LnTag.PaymentHashTag

  def description: Option[LnTag.DescriptionTag]

  def nodeId: Option[LnTag.NodeIdTag]

  def descriptionHash: Option[LnTag.DescriptionHashTag]

  def expiryTime: Option[LnTag.ExpiryTimeTag]

  def cltvExpiry: Option[LnTag.MinFinalCltvExpiry]

  def fallbackAddress: Option[LnTag.FallbackAddressTag]

  def routingInfo: Option[LnTag.RoutingInfo]

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

object LnTaggedFields extends {
  private case class InvoiceTagImpl(
    paymentHash: LnTag.PaymentHashTag,
    description: Option[LnTag.DescriptionTag],
    nodeId: Option[LnTag.NodeIdTag],
    descriptionHash: Option[LnTag.DescriptionHashTag],
    expiryTime: Option[LnTag.ExpiryTimeTag],
    cltvExpiry: Option[LnTag.MinFinalCltvExpiry],
    fallbackAddress: Option[LnTag.FallbackAddressTag],
    routingInfo: Option[LnTag.RoutingInfo]) extends LnTaggedFields {
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
    descriptionOrHash: Either[LnTag.DescriptionTag, LnTag.DescriptionHashTag]): LnTaggedFields = {

    LnTaggedFields.apply(paymentHashTag, descriptionOrHash)
  }

  def apply(
    paymentHash: LnTag.PaymentHashTag,
    descriptionOrHash: Either[LnTag.DescriptionTag, LnTag.DescriptionHashTag],
    nodeId: Option[LnTag.NodeIdTag] = None,
    expiryTime: Option[LnTag.ExpiryTimeTag] = None,
    cltvExpiry: Option[LnTag.MinFinalCltvExpiry] = None,
    fallbackAddress: Option[LnTag.FallbackAddressTag] = None,
    routingInfo: Option[LnTag.RoutingInfo] = None): LnTaggedFields = {

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

  def fromUInt5s(u5s: Vector[UInt5]): LnTaggedFields = {
    @tailrec
    def loop(remaining: List[UInt5], fields: Vector[LnTag]): Vector[LnTag] = {
      remaining match {
        case h :: h1 :: h2 :: t =>

          val prefix = LnTagPrefix.fromUInt5(h)

          //next two 5 bit increments are data_length
          val dataLengthU5s = Vector(h1, h2)

          val dataLength = LnUtil.decodeDataLength(dataLengthU5s)

          //t is the actual possible payload
          val payload: Vector[UInt5] = t.take(dataLength.toInt).toVector

          val tag = LnTag.fromLnTagPrefix(prefix.get, payload)

          val newRemaining = t.slice(payload.size, t.size)

          loop(newRemaining, fields.:+(tag))

        case _ :: _ | _ :: _ :: _ =>
          throw new IllegalArgumentException("Failed to parse LnTaggedFields, needs 15bits of meta data to be able to parse")
        case Nil =>
          fields
      }
    }

    val tags = loop(u5s.toList, Vector.empty)

    val paymentHashTag = tags.find(_.isInstanceOf[LnTag.PaymentHashTag])
      .get.asInstanceOf[LnTag.PaymentHashTag]

    val description = tags.find(_.isInstanceOf[LnTag.DescriptionTag])
      .map(_.asInstanceOf[LnTag.DescriptionTag])

    val descriptionHash = tags.find(_.isInstanceOf[LnTag.DescriptionHashTag])
      .map(_.asInstanceOf[LnTag.DescriptionHashTag])

    val nodeId = tags.find(_.isInstanceOf[LnTag.NodeIdTag])
      .map(_.asInstanceOf[LnTag.NodeIdTag])

    val expiryTime = tags.find(_.isInstanceOf[LnTag.ExpiryTimeTag])
      .map(_.asInstanceOf[LnTag.ExpiryTimeTag])

    val cltvExpiry = tags.find(_.isInstanceOf[LnTag.MinFinalCltvExpiry])
      .map(_.asInstanceOf[LnTag.MinFinalCltvExpiry])

    val fallbackAddress = tags.find(_.isInstanceOf[LnTag.FallbackAddressTag])
      .map(_.asInstanceOf[LnTag.FallbackAddressTag])

    val routingInfo = tags.find(_.isInstanceOf[LnTag.RoutingInfo])
      .map(_.asInstanceOf[LnTag.RoutingInfo])

    val d: Either[LnTag.DescriptionTag, LnTag.DescriptionHashTag] = {
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

    LnTaggedFields(
      paymentHash = paymentHashTag,
      descriptionOrHash = d,
      nodeId = nodeId,
      expiryTime = expiryTime,
      cltvExpiry = cltvExpiry,
      fallbackAddress = fallbackAddress,
      routingInfo = routingInfo)

  }
}