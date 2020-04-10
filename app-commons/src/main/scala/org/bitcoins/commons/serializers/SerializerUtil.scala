package org.bitcoins.commons.serializers

import play.api.libs.json._

sealed abstract class SerializerUtil {

  def processJsNumberBigInt[T](numFunc: BigInt => T)(
      json: JsValue): JsResult[T] = json match {
    case JsNumber(nDecimal) =>
      val nOpt = nDecimal.toBigIntExact
      nOpt match {
        case Some(t) => JsSuccess(numFunc(t))
        case None =>
          JsError(s"Could not parsed expected t from given string $nDecimal")
      }
    case err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
        _: JsObject) =>
      buildJsErrorMsg("jsnumber", err)
  }

  def buildJsErrorMsg(expected: String, err: JsValue): JsError = {
    JsError(s"error.expected.$expected, got ${Json.toJson(err).toString()}")
  }

  def buildErrorMsg(expected: String, err: Any): JsError = {
    JsError(s"error.expected.$expected, got ${err.toString}")
  }

  // For use in implementing reads method of Reads[T] where T is constructed from a JsNumber via numFunc
  def processJsNumber[T](numFunc: BigDecimal => T)(json: JsValue): JsResult[T] =
    json match {
      case JsNumber(n) => JsSuccess(numFunc(n))
      case err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
          _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsnumber", err)
    }

  def processJsObject[T](f: JsObject => T)(json: JsValue): JsResult[T] = {
    json match {
      case obj: JsObject => JsSuccess(f(obj))
      case err @ (JsNull | _: JsBoolean | _: JsString | _: JsArray |
          _: JsNumber) =>
        SerializerUtil.buildJsErrorMsg("jsobject", err)
    }
  }

  // For use in implementing reads method of Reads[T] where T is constructed from a JsString via strFunc
  def processJsString[T](strFunc: String => T)(json: JsValue): JsResult[T] =
    json match {
      case JsString(s) => JsSuccess(strFunc(s))
      case err @ (JsNull | _: JsBoolean | _: JsNumber | _: JsArray |
          _: JsObject) =>
        SerializerUtil.buildJsErrorMsg("jsstring", err)
    }

  def processJsStringOpt[T](f: String => Option[T])(
      jsValue: JsValue): JsResult[T] = {
    jsValue match {
      case JsString(key) =>
        val tOpt = f(key)
        tOpt match {
          case Some(t) => JsSuccess(t)
          case None    => SerializerUtil.buildErrorMsg("invalid jsstring", jsValue)
        }
      case err @ (_: JsNumber | _: JsObject | _: JsArray | JsNull |
          _: JsBoolean) =>
        SerializerUtil.buildErrorMsg("jsstring", err)
    }
  }
}

object SerializerUtil extends SerializerUtil
