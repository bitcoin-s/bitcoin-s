package org.bitcoins.lnurl.json

import org.bitcoins.commons.serializers.SerializerUtil
import org.bitcoins.crypto.StringFactory
import play.api.libs.json._

sealed abstract class LnURLTag(override val toString: String)

object LnURLTag extends StringFactory[LnURLTag] {

  case object WithdrawRequest extends LnURLTag("withdrawRequest")
  case object PayRequest extends LnURLTag("payRequest")

  val all: Vector[LnURLTag] = Vector(WithdrawRequest, PayRequest)

  override def fromStringOpt(string: String): Option[LnURLTag] = {
    all.find(_.toString == string)
  }

  override def fromString(string: String): LnURLTag = {
    fromStringOpt(string).getOrElse(
      sys.error(s"Could not find a LnURLTag for string $string"))
  }

  implicit val LnURLTagTagReads: Reads[LnURLTag] = (json: JsValue) =>
    SerializerUtil.processJsStringOpt[LnURLTag](fromStringOpt)(json)

  implicit val LnURLTagTagWrites: Writes[LnURLTag] = (tag: LnURLTag) =>
    JsString(tag.toString)
}

sealed abstract class SuccessActionTag(override val toString: String)

object SuccessActionTag extends StringFactory[SuccessActionTag] {

  case object Message extends SuccessActionTag("message")
  case object URL extends SuccessActionTag("url")
  case object AES extends SuccessActionTag("aes")

  val all: Vector[SuccessActionTag] = Vector(Message, URL, AES)

  override def fromStringOpt(string: String): Option[SuccessActionTag] = {
    all.find(_.toString == string)
  }

  override def fromString(string: String): SuccessActionTag = {
    fromStringOpt(string).getOrElse(
      sys.error(s"Could not find a SuccessActionTag for string $string"))
  }

  implicit val SuccessActionTagReads: Reads[SuccessActionTag] =
    (json: JsValue) =>
      SerializerUtil.processJsStringOpt[SuccessActionTag](fromStringOpt)(json)

  implicit val SuccessActionTagWrites: Writes[SuccessActionTag] =
    (tag: SuccessActionTag) => JsString(tag.toString)
}
