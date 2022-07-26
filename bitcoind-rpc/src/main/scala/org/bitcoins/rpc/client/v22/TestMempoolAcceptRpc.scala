package org.bitcoins.rpc.client.v22

import org.bitcoins.commons.jsonmodels.bitcoind.TestMempoolAcceptResultPostV22
import org.bitcoins.commons.serializers.JsonSerializers.{
  testMempoolAcceptResultReadsPostV22,
  transactionWrites
}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.rpc.client.common.Client
import play.api.libs.json.Json

import scala.concurrent.Future

trait TestMempoolAcceptRpc { self: Client =>

  def testMempoolAccept(
      transaction: Vector[Transaction],
      maxFeeRate: Double = 0.10): Future[
    Vector[TestMempoolAcceptResultPostV22]] = {
    bitcoindCall[Vector[TestMempoolAcceptResultPostV22]](
      "testmempoolaccept",
      List(Json.toJson(transaction), Json.toJson(maxFeeRate)))
  }

}
