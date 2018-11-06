package org.bitcoins.rpc.client

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.rpc.jsonmodels.{GetMemPoolEntryResult, GetMemPoolInfoResult, GetMemPoolResult}
import org.bitcoins.rpc.serializers.JsonReaders._
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.{JsBoolean, JsString}

import scala.concurrent.Future

protected trait MempoolCalls extends Client with BitcoindCall {

  def getMemPoolAncestors(
                           txid: DoubleSha256Digest): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getMemPoolAncestorsVerbose(txid: DoubleSha256Digest): Future[Map[DoubleSha256Digest, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256Digest, GetMemPoolResult]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getMemPoolDescendants(
                             txid: DoubleSha256Digest): Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getMemPoolDescendantsVerbose(txid: DoubleSha256Digest): Future[Map[DoubleSha256Digest, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256Digest, GetMemPoolResult]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(true)))
  }

  def getMemPoolEntry(
                       txid: DoubleSha256Digest): Future[GetMemPoolEntryResult] = {
    bitcoindCall[GetMemPoolEntryResult](
      "getmempoolentry",
      List(JsString(txid.hex)))
  }

  def getMemPoolInfo: Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getRawMemPool: Future[Vector[DoubleSha256Digest]] = {
    bitcoindCall[Vector[DoubleSha256Digest]](
      "getrawmempool",
      List(JsBoolean(false)))
  }

  def getRawMemPoolWithTransactions: Future[Map[DoubleSha256Digest, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256Digest, GetMemPoolResult]](
      "getrawmempool",
      List(JsBoolean(true)))
  }

  def saveMemPool(): Future[Unit] = {
    bitcoindCall[Unit]("savemempool")
  }



}
