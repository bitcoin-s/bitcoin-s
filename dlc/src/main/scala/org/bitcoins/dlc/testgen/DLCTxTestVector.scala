package org.bitcoins.dlc.testgen

import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import play.api.libs.json.{Format, JsResult, JsValue, Json}

import scala.concurrent.{ExecutionContext, Future}

case class DLCTxTestVector[Outcome <: DLCOutcomeType](
    inputs: ValidTestInputs[Outcome],
    txs: DLCTransactions)
    extends TestVector {

  override def toJson: JsValue =
    Json.toJson(this)(DLCTxTestVectorHelper[Outcome]().dlcTxTestVectorFormat)
}

case class DLCTxTestVectorHelper[Outcome <: DLCOutcomeType]()
    extends TestVectorParser[DLCTxTestVector[Outcome]] {

  def fromInputs(inputs: ValidTestInputs[Outcome])(implicit
      ec: ExecutionContext): Future[DLCTxTestVector[Outcome]] = {
    inputs.buildTransactions.map(txs => DLCTxTestVector(inputs, txs))
  }

  private val helper = TestVectorHelper[Outcome]()
  import helper.validTestInputsFormat
  import helper.dlcTransactionsFormat

  implicit val dlcTxTestVectorFormat: Format[DLCTxTestVector[Outcome]] =
    Json.format[DLCTxTestVector[Outcome]]

  override def fromJson(json: JsValue): JsResult[DLCTxTestVector[Outcome]] = {
    json.validate[DLCTxTestVector[Outcome]]
  }
}
