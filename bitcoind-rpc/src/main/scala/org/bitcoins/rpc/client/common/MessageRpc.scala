package org.bitcoins.rpc.client.common

import org.bitcoins.core.crypto.ECPrivateKeyUtil
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.crypto.ECPrivateKey
import play.api.libs.json.JsString

import scala.concurrent.Future

/**
  * RPC calls related to the message signing functionality
  * in bitcoind
  */
trait MessageRpc { self: Client =>

  def signMessage(address: P2PKHAddress, message: String): Future[String] = {
    bitcoindCall[String]("signmessage",
                         List(JsString(address.value), JsString(message)))
  }

  def signMessageWithPrivKey(
      key: ECPrivateKey,
      message: String): Future[String] = {
    bitcoindCall[String](
      "signmessagewithprivkey",
      List(JsString(ECPrivateKeyUtil.toWIF(key, network)), JsString(message)))
  }

  def verifyMessage(
      address: P2PKHAddress,
      signature: String,
      message: String): Future[Boolean] = {
    bitcoindCall[Boolean](
      "verifymessage",
      List(JsString(address.value), JsString(signature), JsString(message)))
  }
}
