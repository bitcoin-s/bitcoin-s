package org.bitcoins.dlc.testgen

import play.api.libs.json.{JsResult, JsValue}

trait TestVector {
  def toJson: JsValue
}

trait TestVectorParser[T <: TestVector] {
  def fromJson(json: JsValue): JsResult[T]
}
