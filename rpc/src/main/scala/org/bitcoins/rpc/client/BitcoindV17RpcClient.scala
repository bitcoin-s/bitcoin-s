package org.bitcoins.rpc.client

import akka.stream.ActorMaterializer
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.rpc.config.BitcoindInstance
import org.bitcoins.rpc.serializers.BitcoindJsonReaders._
import play.api.libs.json.{ JsNumber, JsString }

import scala.concurrent.Future

class BitcoindV17RpcClient(override protected val instance: BitcoindInstance)(
  implicit
  m: ActorMaterializer) extends BitcoindRpcClient(instance) {

  def getReceivedByLabel(
    account: String,
    confirmations: Int = 1): Future[Bitcoins] = {
    bitcoindCall[Bitcoins](
      "getreceivedbylabel",
      List(JsString(account), JsNumber(confirmations)))
  }
}

