package org.bitcoins.dlc.testgen

import play.api.libs.json.{Format, JsResult, JsValue, Json}

import scala.concurrent.{ExecutionContext, Future}

case class DLCTxTestVector(inputs: ValidTestInputs, txs: DLCTransactions)
    extends TestVector {

  override def toJson: JsValue =
    Json.toJson(this)(DLCTxTestVector.dlcTxTestVectorFormat)
}

object DLCTxTestVector extends TestVectorParser[DLCTxTestVector] {

  def fromInputs(inputs: ValidTestInputs)(implicit
      ec: ExecutionContext): Future[DLCTxTestVector] = {
    inputs.buildTransactions.map(txs => DLCTxTestVector(inputs, txs))
  }

  import SuccessTestVector.validTestInputsFormat
  import SuccessTestVector.dlcTransactionsFormat

  implicit val dlcTxTestVectorFormat: Format[DLCTxTestVector] =
    Json.format[DLCTxTestVector]

  override def fromJson(json: JsValue): JsResult[DLCTxTestVector] = {
    json.validate[DLCTxTestVector]
  }
}
