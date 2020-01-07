package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.{UInt5, UInt8}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.ln.util.LnUtil
import org.bitcoins.core.util.Bech32
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * An aggregation of all the individual tagged fields in a [[org.bitcoins.core.protocol.ln.LnInvoice]]
  */
sealed abstract class LnTaggedFields extends NetworkElement {

  require(
    (description.nonEmpty && description.get.string.length < 640) ||
      descriptionHash.nonEmpty,
    "You must supply either a description hash, or a literal description that is 640 characters or less to create an invoice."
  )

  def paymentHash: LnTag.PaymentHashTag

  def secret: Option[LnTag.SecretTag]

  def description: Option[LnTag.DescriptionTag]

  def nodeId: Option[LnTag.NodeIdTag]

  def descriptionHash: Option[LnTag.DescriptionHashTag]

  def expiryTime: Option[LnTag.ExpiryTimeTag]

  def cltvExpiry: Option[LnTag.MinFinalCltvExpiry]

  def fallbackAddress: Option[LnTag.FallbackAddressTag]

  def routingInfo: Option[LnTag.RoutingInfo]

  def features: Option[LnTag.FeaturesTag]

  lazy val data: Vector[UInt5] = Vector(Some(paymentHash),
                                        description,
                                        nodeId,
                                        descriptionHash,
                                        secret,
                                        expiryTime,
                                        cltvExpiry,
                                        fallbackAddress,
                                        routingInfo,
                                        features)
    .filter(_.isDefined)
    .flatMap(_.get.data)

  override def bytes: ByteVector = {
    val u8s = Bech32.from5bitTo8bit(data)
    UInt8.toBytes(u8s)
  }

  override def toString: String = {
    val b = new mutable.StringBuilder()

    val string = Bech32.encode5bitToString(data)
    b.append(string)

    b.toString()
  }
}

object LnTaggedFields {
  private case class InvoiceTagImpl(
      paymentHash: LnTag.PaymentHashTag,
      description: Option[LnTag.DescriptionTag],
      nodeId: Option[LnTag.NodeIdTag],
      descriptionHash: Option[LnTag.DescriptionHashTag],
      expiryTime: Option[LnTag.ExpiryTimeTag],
      secret: Option[LnTag.SecretTag],
      cltvExpiry: Option[LnTag.MinFinalCltvExpiry],
      fallbackAddress: Option[LnTag.FallbackAddressTag],
      routingInfo: Option[LnTag.RoutingInfo],
      features: Option[LnTag.FeaturesTag])
      extends LnTaggedFields

  /**
    * According to BOLT11 the required fields in a LnInvoice are a payment hash
    * and either a description, or the hash of the description.
    */
  def apply(
      paymentHash: LnTag.PaymentHashTag,
      descriptionOrHash: Either[LnTag.DescriptionTag, LnTag.DescriptionHashTag],
      secret: Option[LnTag.SecretTag] = None,
      nodeId: Option[LnTag.NodeIdTag] = None,
      expiryTime: Option[LnTag.ExpiryTimeTag] = None,
      cltvExpiry: Option[LnTag.MinFinalCltvExpiry] = None,
      fallbackAddress: Option[LnTag.FallbackAddressTag] = None,
      routingInfo: Option[LnTag.RoutingInfo] = None,
      features: Option[LnTag.FeaturesTag] = None): LnTaggedFields = {

    val (description, descriptionHash): (
        Option[LnTag.DescriptionTag],
        Option[LnTag.DescriptionHashTag]) = {

      descriptionOrHash match {
        case Left(description) =>
          (Some(description), None)
        case Right(hash) =>
          (None, Some(hash))
      }
    }

    InvoiceTagImpl(
      paymentHash = paymentHash,
      secret = secret,
      description = description,
      nodeId = nodeId,
      descriptionHash = descriptionHash,
      expiryTime = expiryTime,
      cltvExpiry = cltvExpiry,
      fallbackAddress = fallbackAddress,
      routingInfo = routingInfo,
      features = features
    )
  }

  /** This is intended to parse all of the [[org.bitcoins.core.protocol.ln.LnTaggedFields LnTaggedFields]]
    * from the tagged part of the ln invoice. This should only be called
    * if other information has already been remove from the invoice
    * like the [[LnHumanReadablePart]]
    * @param u5s payload of the tagged fields in the invoice
    * @return
    */
  def fromUInt5s(u5s: Vector[UInt5]): LnTaggedFields = {
    @tailrec
    def loop(remaining: List[UInt5], fields: Vector[LnTag]): Vector[LnTag] = {
      remaining match {
        case h :: h1 :: h2 :: t =>
          val prefix = LnTagPrefix.fromUInt5(h)

          //next two 5 bit increments are data_length
          val dataLengthU5s = List(h1, h2)

          val dataLength = LnUtil.decodeDataLength(dataLengthU5s)

          //t is the actual possible payload
          val payload: Vector[UInt5] = t.take(dataLength.toInt).toVector

          val tag = LnTag.fromLnTagPrefix(prefix.get, payload)

          val newRemaining = t.slice(payload.size, t.size)

          loop(newRemaining, fields.:+(tag))
        case Nil =>
          fields
        case _ :: _ | _ :: _ :: _ =>
          throw new IllegalArgumentException(
            "Failed to parse LnTaggedFields, needs 15bits of meta data to be able to parse")
      }
    }

    val tags = loop(u5s.toList, Vector.empty)

    def getTag[T <: LnTag: ClassTag]: Option[T] = {
      tags
        .map {
          case t: T => Some(t)
          case _    => None
        }
        .find(_.isDefined)
        .flatten
    }

    val paymentHashTag = getTag[LnTag.PaymentHashTag].getOrElse(
      throw new IllegalArgumentException(
        s"Payment hash must be defined in a LnInvoice")
    )

    val secret = getTag[LnTag.SecretTag]

    val description = getTag[LnTag.DescriptionTag]

    val descriptionHash = getTag[LnTag.DescriptionHashTag]

    val nodeId = getTag[LnTag.NodeIdTag]

    val expiryTime = getTag[LnTag.ExpiryTimeTag]

    val cltvExpiry = getTag[LnTag.MinFinalCltvExpiry]

    val fallbackAddress = getTag[LnTag.FallbackAddressTag]

    val routingInfo = getTag[LnTag.RoutingInfo]

    val features = getTag[LnTag.FeaturesTag]

    val d: Either[LnTag.DescriptionTag, LnTag.DescriptionHashTag] = {
      if (description.isDefined && descriptionHash.isDefined) {
        throw new IllegalArgumentException(
          s"Cannot have both description and description hash")
      } else if (description.isEmpty && descriptionHash.isEmpty) {
        throw new IllegalArgumentException(
          s"One of description / description hash fields must be defind")
      } else if (description.isDefined) {
        Left(description.get)
      } else {
        Right(descriptionHash.get)
      }
    }

    LnTaggedFields(
      paymentHash = paymentHashTag,
      secret = secret,
      descriptionOrHash = d,
      nodeId = nodeId,
      expiryTime = expiryTime,
      cltvExpiry = cltvExpiry,
      fallbackAddress = fallbackAddress,
      routingInfo = routingInfo,
      features = features
    )

  }
}
