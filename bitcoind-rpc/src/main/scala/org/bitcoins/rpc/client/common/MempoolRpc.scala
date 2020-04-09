package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.rpc.client.common.BitcoindVersion._
import play.api.libs.json.{JsBoolean, JsString}

import scala.concurrent.Future

/**
  * This trait defines RPC calls related to
  * the mempool of a Bitcoin Core node. The
  * mempool contains all unconfirmed transactions.
  */
trait MempoolRpc { self: Client =>

  def getMemPoolAncestors(
      txid: DoubleSha256DigestBE): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getMemPoolAncestors(
      txid: DoubleSha256Digest): Future[Vector[DoubleSha256DigestBE]] = {
    getMemPoolAncestors(txid.flip)
  }

  def getMemPoolAncestorsVerbose(txid: DoubleSha256DigestBE): Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {

    self.version match {
      case V19 | Experimental | Unknown =>
        bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV19]](
          "getmempoolancestors",
          List(JsString(txid.hex), JsBoolean(true)))
      case V16 | V17 | V18 =>
        bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPreV19]](
          "getmempoolancestors",
          List(JsString(txid.hex), JsBoolean(true)))
    }
  }

  def getMemPoolAncestorsVerbose(txid: DoubleSha256Digest): Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    getMemPoolAncestorsVerbose(txid.flip)
  }

  def getMemPoolDescendants(
      txid: DoubleSha256DigestBE): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(false)))
  }

  def getMemPoolDescendants(
      txid: DoubleSha256Digest): Future[Vector[DoubleSha256DigestBE]] = {
    getMemPoolDescendants(txid.flip)
  }

  def getMemPoolDescendantsVerbose(txid: DoubleSha256DigestBE): Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    self.version match {
      case V19 | Experimental | Unknown =>
        bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV19]](
          "getmempooldescendants",
          List(JsString(txid.hex), JsBoolean(true)))
      case V16 | V17 | V18 =>
        bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPreV19]](
          "getmempooldescendants",
          List(JsString(txid.hex), JsBoolean(true)))
    }
  }

  def getMemPoolDescendantsVerbose(txid: DoubleSha256Digest): Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    getMemPoolDescendantsVerbose(txid.flip)
  }

  def getMemPoolEntry(
      txid: DoubleSha256DigestBE): Future[GetMemPoolEntryResult] = {

    self.version match {
      case V19 | Experimental | Unknown =>
        bitcoindCall[GetMemPoolEntryResultPostV19]("getmempoolentry",
                                                   List(JsString(txid.hex)))
      case V16 | V17 | V18 =>
        bitcoindCall[GetMemPoolEntryResultPreV19]("getmempoolentry",
                                                  List(JsString(txid.hex)))
    }

  }

  def getMemPoolEntry(
      txid: DoubleSha256Digest): Future[GetMemPoolEntryResult] = {
    getMemPoolEntry(txid.flip)
  }

  def getMemPoolInfo: Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getRawMemPool: Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]]("getrawmempool",
                                               List(JsBoolean(false)))
  }

  def getRawMemPoolWithTransactions: Future[
    Map[DoubleSha256DigestBE, GetMemPoolResult]] = {

    self.version match {
      case V19 | Experimental | Unknown =>
        bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV19]](
          "getrawmempool",
          List(JsBoolean(true)))
      case V16 | V17 | V18 =>
        bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPreV19]](
          "getrawmempool",
          List(JsBoolean(true)))
    }

  }

  def saveMemPool(): Future[Unit] = {
    bitcoindCall[Unit]("savemempool")
  }

}
