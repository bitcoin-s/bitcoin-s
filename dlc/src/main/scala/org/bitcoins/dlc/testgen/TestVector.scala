package org.bitcoins.dlc.testgen

import play.api.libs.json.{JsResult, JsValue, Json}

trait TestVector {
  def toJson: JsValue

  def toJsonStr: String = {
    Json.prettyPrint(toJson)
  }
}

trait TestVectorParser[T <: TestVector] {
  def fromJson(json: JsValue): JsResult[T]

  def fromString(str: String): JsResult[T] = {
    fromJson(Json.parse(str))
  }
}
