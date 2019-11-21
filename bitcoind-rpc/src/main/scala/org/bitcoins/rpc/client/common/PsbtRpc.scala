package org.bitcoins.rpc.client.common

import org.bitcoins.rpc.jsonmodels.AnalyzePsbtResult
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsString, Json}

import scala.concurrent.Future

/**
  * Set of utilities to analyze, join, and update existing PSBTs
  *
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/rawtransactions/analyzepsbt/]]
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/rawtransactions/joinpsbts/]]
  * @see [[https://bitcoincore.org/en/doc/0.18.0/rpc/rawtransactions/utxoupdatepsbt/]]
  */
trait PsbtRpc {
  self: Client =>

  def analyzePsbt(psbt: String): Future[AnalyzePsbtResult] = {
    bitcoindCall[AnalyzePsbtResult]("analyzepsbt", List(JsString(psbt)))
  }

  def joinPsbts(txs: Seq[String]): Future[String] = {
    bitcoindCall[String]("joinpsbts", List(Json.toJson(txs)))
  }

  def utxoUpdatePsbt(psbt: String): Future[String] = {
    bitcoindCall[String]("utxoupdatepsbt", List(JsString(psbt)))
  }

  def utxoUpdatePsbt(psbt: String, descriptors: Seq[String]): Future[String] = {
    bitcoindCall[String]("utxoupdatepsbt",
                         List(JsString(psbt), Json.toJson(descriptors)))
  }

}
