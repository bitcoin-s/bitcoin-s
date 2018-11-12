package org.bitcoins.rpc.client.v16

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.serializers.BitcoindJsonReaders._
import org.bitcoins.rpc.serializers.BitcoindJsonSerializers._
import play.api.libs.json.{ JsNumber, JsString }

import scala.concurrent.Future

trait V16SendRpc extends Client {

  def move(
    fromAccount: String,
    toAccount: String,
    amount: Bitcoins,
    comment: String = ""): Future[Boolean] = {
    bitcoindCall[Boolean](
      "move",
      List(
        JsString(fromAccount),
        JsString(toAccount),
        JsNumber(amount.toBigDecimal),
        JsNumber(6),
        JsString(comment)))
  }

  def sendFrom(
    fromAccount: String,
    toAddress: BitcoinAddress,
    amount: Bitcoins,
    confirmations: Int = 1,
    comment: String = "",
    toComment: String = ""): Future[DoubleSha256Digest] = {
    bitcoindCall[DoubleSha256Digest](
      "sendfrom",
      List(
        JsString(fromAccount),
        JsString(toAddress.value),
        JsNumber(amount.toBigDecimal),
        JsNumber(confirmations),
        JsString(comment),
        JsString(toComment)))
  }
}
