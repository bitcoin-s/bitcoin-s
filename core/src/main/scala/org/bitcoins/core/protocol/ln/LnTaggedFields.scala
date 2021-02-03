package org.bitcoins.core.protocol.ln

import org.bitcoins.core.number.{UInt5, UInt8}
import org.bitcoins.core.protocol.ln.util.LnUtil
import org.bitcoins.core.util.{Bech32, SeqWrapper}
import org.bitcoins.crypto.NetworkElement
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

/** An aggregation of all the individual tagged fields in a [[org.bitcoins.core.protocol.ln.LnInvoice]]
  */
sealed abstract class LnTaggedFields
    extends SeqWrapper[LnTag]
    with NetworkElement {

  require(tag[LnTag.PaymentHashTag].nonEmpty, "You must supply a payment hash")
  require(
    (description.nonEmpty && description.get.string.length < 640) ||
      descriptionHash.nonEmpty,
    "You must supply either a description hash, or a literal description that is 640 characters or less to create an invoice."
  )
  require(!(description.nonEmpty && descriptionHash.nonEmpty),
          "Cannot have both description and description hash")

  def tags: Vector[LnTag]

  override protected lazy val wrapped: Vector[LnTag] = tags

  def tag[T <: LnTag: ClassTag]: Option[T] =
    tags.collectFirst { case t: T =>
      t
    }

  lazy val paymentHash: LnTag.PaymentHashTag =
    tag[LnTag.PaymentHashTag].get

  lazy val secret: Option[LnTag.SecretTag] =
    tag[LnTag.SecretTag]

  lazy val description: Option[LnTag.DescriptionTag] =
    tag[LnTag.DescriptionTag]

  lazy val nodeId: Option[LnTag.NodeIdTag] =
    tag[LnTag.NodeIdTag]

  lazy val descriptionHash: Option[LnTag.DescriptionHashTag] =
    tag[LnTag.DescriptionHashTag]

  lazy val expiryTime: Option[LnTag.ExpiryTimeTag] =
    tag[LnTag.ExpiryTimeTag]

  lazy val cltvExpiry: Option[LnTag.MinFinalCltvExpiry] =
    tag[LnTag.MinFinalCltvExpiry]

  lazy val fallbackAddress: Option[LnTag.FallbackAddressTag] =
    tag[LnTag.FallbackAddressTag]

  lazy val routingInfo: Option[LnTag.RoutingInfo] =
    tag[LnTag.RoutingInfo]

  lazy val features: Option[LnTag.FeaturesTag] =
    tag[LnTag.FeaturesTag]

  lazy val data: Vector[UInt5] = tags.flatMap(_.data)

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
  private case class InvoiceTagImpl(tags: Vector[LnTag]) extends LnTaggedFields

  /** According to BOLT11 the required fields in a LnInvoice are a payment hash
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

    val tags = Vector(Some(paymentHash),
                      description,
                      nodeId,
                      descriptionHash,
                      expiryTime,
                      cltvExpiry,
                      fallbackAddress,
                      routingInfo,
                      features,
                      secret).flatten
    InvoiceTagImpl(tags)
  }

  def apply(tags: Vector[LnTag]): LnTaggedFields = InvoiceTagImpl(tags)

  /** This is intended to parse all of the [[org.bitcoins.core.protocol.ln.LnTaggedFields LnTaggedFields]]
    * from the tagged part of the ln invoice. This should only be called
    * if other information has already been remove from the invoice
    * like the [[LnHumanReadablePart]]
    * @param u5s payload of the tagged fields in the invoice
    * @return
    */
  def fromUInt5s(u5s: Vector[UInt5]): LnTaggedFields = {
    @tailrec
    def loop(remaining: Vector[UInt5], fields: Vector[LnTag]): Vector[LnTag] = {
      remaining match {
        case h +: h1 +: h2 +: t =>
          val prefix = LnTagPrefix
            .fromUInt5(h)
            .getOrElse(
              throw new RuntimeException("Unknown LN invoice tag prefix"))

          //next two 5 bit increments are data_length
          val dataLengthU5s = List(h1, h2)

          val dataLength = LnUtil.decodeDataLength(dataLengthU5s)

          //t is the actual possible payload
          val payload: Vector[UInt5] = t.take(dataLength.toInt).toVector

          val tag = LnTag.fromLnTagPrefix(prefix, payload)

          val newRemaining = t.slice(payload.size, t.size)

          loop(newRemaining, fields :+ tag)
        case _ +: _ | _ +: _ +: _ =>
          throw new IllegalArgumentException(
            "Failed to parse LnTaggedFields, needs 15bits of meta data to be able to parse")
        case _: Vector[_] =>
          fields
      }
    }

    val tags = loop(u5s, Vector.empty)

    InvoiceTagImpl(tags)

  }
}
