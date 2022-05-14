package org.bitcoins.lnurl.json

import org.bitcoins.core.protocol.ln.currency.MilliSatoshis
import org.bitcoins.lnurl.json.LnURLTag._
import play.api.libs.json._
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.core.protocol.ln.LnInvoice

import java.net._

sealed abstract class LnURLJsonModel

sealed abstract class LnURLResponse extends LnURLJsonModel {
  def tag: LnURLTag
  def callback: URL
}

case class LnURLStatus(status: String, reason: Option[String])
    extends LnURLJsonModel

object LnURLJsonModels {

  implicit val LnURLStatusReads: Reads[LnURLStatus] = Json.reads[LnURLStatus]

  case class LnURLSuccessAction(
      tag: SuccessActionTag,
      message: Option[String],
      description: Option[String],
      url: Option[URL],
      ciphertext: Option[String],
      iv: Option[String])

  implicit val LnURLSuccessActionReads: Reads[LnURLSuccessAction] =
    Json.reads[LnURLSuccessAction]

  case class LnURLPayResponse(
      callback: URL,
      maxSendable: MilliSatoshis,
      minSendable: MilliSatoshis,
      private val metadata: String)
      extends LnURLResponse {
    override val tag: LnURLTag = PayRequest
    lazy val metadataJs: JsValue = Json.parse(metadata)
  }

  implicit val LnURLPayResponseReads: Reads[LnURLPayResponse] =
    Json.reads[LnURLPayResponse]

  case class LnURLPayInvoice(
      pr: LnInvoice,
      successAction: Option[LnURLSuccessAction])
      extends LnURLJsonModel

  implicit val LnURLPayInvoiceReads: Reads[LnURLPayInvoice] =
    Json.reads[LnURLPayInvoice]

  case class LnURLWithdrawResponse(
      callback: URL,
      k1: String,
      defaultDescription: String,
      minWithdrawable: MilliSatoshis,
      maxWithdrawable: MilliSatoshis
  ) extends LnURLResponse {
    override val tag: LnURLTag = WithdrawRequest
  }

  implicit val LnURLWithdrawResponseReads: Reads[LnURLWithdrawResponse] =
    Json.reads[LnURLWithdrawResponse]

  implicit val LnURLResponseReads: Reads[LnURLResponse] = {
    case other @ (JsNull | _: JsBoolean | JsNumber(_) | JsString(_) | JsArray(
          _)) =>
      throw new IllegalArgumentException(s"Expected JsObject, got $other")
    case obj: JsObject =>
      obj.value.get("tag") match {
        case None =>
          throw new RuntimeException(s"Error parsing json, no tag, got $obj")
        case Some(tagJs) =>
          tagJs.validate[LnURLTag] match {
            case JsError(errors) =>
              throw new IllegalArgumentException(
                s"Invalid json, got $obj, errors ${errors.mkString("\n")}")
            case JsSuccess(tag, _) =>
              tag match {
                case PayRequest      => obj.validate[LnURLPayResponse]
                case WithdrawRequest => obj.validate[LnURLWithdrawResponse]
              }
          }
      }
  }
}
