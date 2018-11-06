package org.bitcoins.rpc.client.v16

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.Client
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsNumber, JsString}

import scala.concurrent.Future

/**
  * RPC calls related to transaction sending
  * specific to Bitcoin Core 0.16.
  */
trait V16SendRpc { self: Client =>

  def move(
      fromAccount: String,
      toAccount: String,
      amount: Bitcoins,
      comment: String = ""): Future[Boolean] = {
    bitcoindCall[Boolean]("move",
                          List(JsString(fromAccount),
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
      toComment: String = ""): Future[DoubleSha256DigestBE] = {
    bitcoindCall[DoubleSha256DigestBE](
      "sendfrom",
      List(JsString(fromAccount),
           JsString(toAddress.value),
           JsNumber(amount.toBigDecimal),
           JsNumber(confirmations),
           JsString(comment),
           JsString(toComment))
    )
  }
}
