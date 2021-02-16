package org.bitcoins.dlc.testgen

import play.api.libs.json.{Format, JsResult, JsValue, Json}

case class DLCTxTestVector(inputs: ValidTestInputs, txs: DLCTransactions)
    extends TestVector {

  override def toJson: JsValue =
    Json.toJson(this)(DLCTxTestVector.dlcTxTestVectorFormat)
}

object DLCTxTestVector extends TestVectorParser[DLCTxTestVector] {

  def fromInputs(inputs: ValidTestInputs): DLCTxTestVector = {
    DLCTxTestVector(inputs, inputs.buildTransactions)
  }

  import SuccessTestVector.validTestInputsFormat
  import SuccessTestVector.dlcTransactionsFormat

  implicit val dlcTxTestVectorFormat: Format[DLCTxTestVector] =
    Json.format[DLCTxTestVector]

  override def fromJson(json: JsValue): JsResult[DLCTxTestVector] = {
    json.validate[DLCTxTestVector]
  }
}
