package org.bitcoins.rpc.client.v22

import org.bitcoins.commons.jsonmodels.bitcoind.TestMempoolAcceptResultPostV22
import org.bitcoins.commons.serializers.JsonSerializers.{
  testMempoolAcceptResultReadsPostV22,
  transactionWrites
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.rpc.client.common.Client
import play.api.libs.json.{JsArray, JsBoolean, Json}

import scala.concurrent.Future

trait TestMempoolAcceptRpc { self: Client =>

  /**  def testMempoolAccept(
    *          transaction: Transaction,
    *          allowHighFees: Boolean = false): Future[TestMempoolAcceptResult] = {
    *        self.version.flatMap {
    *          case BitcoindVersion.V22 | BitcoindVersion.V23 |
    *              BitcoindVersion.Unknown =>
    *            bitcoindCall[Vector[TestMempoolAcceptResultPostV22]](
    *              "testmempoolaccept",
    *              List(JsArray(Vector(Json.toJson(transaction))),
    *                   JsBoolean(allowHighFees)))
    *              .map(_.head)
    *          case BitcoindVersion.V17 | BitcoindVersion.V18 | BitcoindVersion.V19 |
    *              BitcoindVersion.V20 | BitcoindVersion.V21 |
    *              BitcoindVersion.Experimental =>
    *            bitcoindCall[Vector[TestMempoolAcceptResultPreV22]](
    *              "testmempoolaccept",
    *              List(JsArray(Vector(Json.toJson(transaction))),
    *                   JsBoolean(allowHighFees)))
    *              .map(_.head)
    *        }
    *      }
    */

  /**    def testMempoolAccept(transaction: Transaction,
    *                             allowHighFees: Boolean = false): Future[TestMempoolAcceptResultPreV22] = {
    *        bitcoindCall[Vector[TestMempoolAcceptResultPreV22]](
    *          "testmempoolaccept",
    *          List(JsArray(Vector(Json.toJson(transaction))), JsBoolean(allowHighFees)))
    *          .map(_.head)
    *      }
    */

  def testMempoolAccept(
      transaction: Transaction,
      allowHighFees: Boolean = false): Future[
    TestMempoolAcceptResultPostV22] = {
    bitcoindCall[Vector[TestMempoolAcceptResultPostV22]](
      "testmempoolaccept",
      List(JsArray(Vector(Json.toJson(transaction))), JsBoolean(allowHighFees)))
      .map(_.head)
  }

}
