package org.bitcoins.rpc.client.v17

import org.bitcoins.rpc.client.common.Client

trait V17AssortedRpc { self: Client =>

  /**  def testMempoolAccept(
    *      transaction: Transaction,
    *      allowHighFees: Boolean = false): Future[TestMempoolAcceptResult] = {
    *    Self.version.flatMap {
    *      case BitcoindVersion.V22 | BitcoindVersion.V23 |
    *          BitcoindVersion.Unknown =>
    *        bitcoindCall[Vector[TestMempoolAcceptResultPostV22]](
    *          "testmempoolaccept",
    *          List(JsArray(Vector(Json.toJson(transaction))),
    *               JsBoolean(allowHighFees)))
    *          .map(_.head)
    *      case BitcoindVersion.V17 | BitcoindVersion.V18 | BitcoindVersion.V19 |
    *          BitcoindVersion.V20 | BitcoindVersion.V21 |
    *          BitcoindVersion.Experimental =>
    *        bitcoindCall[Vector[TestMempoolAcceptResultPreV22]](
    *          "testmempoolaccept",
    *          List(JsArray(Vector(Json.toJson(transaction))),
    *               JsBoolean(allowHighFees)))
    *          .map(_.head)
    *    }
    *  }
    */

  /**  def testMempoolAccept(transaction: Transaction,
    *                         allowHighFees: Boolean = false): Future[TestMempoolAcceptResult] = {
    *    bitcoindCall[Vector[TestMempoolAcceptResult]](
    *      "testmempoolaccept",
    *      List(JsArray(Vector(Json.toJson(transaction))), JsBoolean(allowHighFees)))
    *      .map(_.head)
    *  }
    */
}
