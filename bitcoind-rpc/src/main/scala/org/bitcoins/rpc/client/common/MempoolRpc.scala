package org.bitcoins.rpc.client.common

import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.rpc.BitcoindException
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import play.api.libs.json.{JsArray, JsBoolean, JsString, Json}

import java.nio.file.Path
import scala.concurrent.Future

/** This trait defines RPC calls related to the mempool of a Bitcoin Core node.
  * The mempool contains all unconfirmed transactions.
  */
trait MempoolRpc { self: Client =>

  def getMemPoolAncestors(
      txid: DoubleSha256DigestBE
  ): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(false))
    )
  }

  def getMemPoolAncestors(
      txid: DoubleSha256Digest
  ): Future[Vector[DoubleSha256DigestBE]] = {
    getMemPoolAncestors(txid.flip)
  }

  def getMemPoolAncestorsVerbose(
      txid: DoubleSha256DigestBE
  ): Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV23]](
      "getmempoolancestors",
      List(JsString(txid.hex), JsBoolean(true))
    )
  }

  def getMemPoolAncestorsVerbose(
      txid: DoubleSha256Digest
  ): Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    getMemPoolAncestorsVerbose(txid.flip)
  }

  def getMemPoolDescendants(
      txid: DoubleSha256DigestBE
  ): Future[Vector[DoubleSha256DigestBE]] = {
    bitcoindCall[Vector[DoubleSha256DigestBE]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(false))
    )
  }

  def getMemPoolDescendants(
      txid: DoubleSha256Digest
  ): Future[Vector[DoubleSha256DigestBE]] = {
    getMemPoolDescendants(txid.flip)
  }

  def getMemPoolDescendantsVerbose(
      txid: DoubleSha256DigestBE
  ): Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV23]](
      "getmempooldescendants",
      List(JsString(txid.hex), JsBoolean(true))
    )
  }

  def getMemPoolDescendantsVerbose(
      txid: DoubleSha256Digest
  ): Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    getMemPoolDescendantsVerbose(txid.flip)
  }

  def getMemPoolEntry(
      txid: DoubleSha256DigestBE
  ): Future[GetMemPoolEntryResult] = {
    bitcoindCall[GetMemPoolEntryResultPostV23](
      "getmempoolentry",
      List(JsString(txid.hex))
    )
  }

  def getMemPoolEntry(
      txid: DoubleSha256Digest
  ): Future[GetMemPoolEntryResult] = {
    getMemPoolEntry(txid.flip)
  }

  def getMemPoolEntryOpt(
      txid: DoubleSha256Digest
  ): Future[Option[GetMemPoolEntryResult]] = {
    getMemPoolEntryOpt(txid.flip)
  }

  def getMemPoolEntryOpt(
      txid: DoubleSha256DigestBE
  ): Future[Option[GetMemPoolEntryResult]] = {
    getMemPoolEntry(txid)
      .map(Some(_))
      .recover { case _: BitcoindException.InvalidAddressOrKey =>
        None
      }
  }

  def getMemPoolInfo: Future[GetMemPoolInfoResult] = {
    bitcoindCall[GetMemPoolInfoResult]("getmempoolinfo")
  }

  def getRawMempoolTxIds(): Future[GetRawMempoolTxIds] = {
    bitcoindCall[GetRawMempoolTxIds](
      "getrawmempool",
      List(JsBoolean(false))
    )
  }

  def getRawMempoolVerbose(): Future[GetRawMempoolVerbose] = {
    bitcoindCall[GetRawMempoolVerbose](
      "getrawmempool",
      List(JsBoolean(true))
    )
  }
  def getRawMemPool(verbose: Boolean = false): Future[GetRawMempoolResult] = {
    if (verbose) {
      getRawMempoolVerbose()
    } else {
      getRawMempoolTxIds()
    }

  }

  def getRawMemPoolWithTransactions
      : Future[Map[DoubleSha256DigestBE, GetMemPoolResult]] = {
    bitcoindCall[Map[DoubleSha256DigestBE, GetMemPoolResultPostV23]](
      "getrawmempool",
      List(JsBoolean(true))
    )
  }

  def saveMemPool(): Future[Unit] = {
    bitcoindCall[Unit]("savemempool")
  }

  def importMempool(path: Path): Future[Unit] = {
    bitcoindCall[Unit]("importmempool", List(JsString(path.toString)))
  }

  def testMempoolAccept(
      transaction: Vector[Transaction],
      maxFeeRate: Double = 0.10
  ): Future[Vector[TestMempoolAcceptResultPostV24]] = {
    bitcoindCall[Vector[TestMempoolAcceptResultPostV24]](
      "testmempoolaccept",
      List(Json.toJson(transaction), Json.toJson(maxFeeRate))
    )
  }

  def getTxSpendingPrevOut(
      prevout: TransactionOutPoint
  ): Future[GetTxSpendingPrevOutResult] = {
    getTxSpendingPrevOut(Vector(prevout)).map(_.head)
  }

  def getTxSpendingPrevOut(
      prevouts: Vector[TransactionOutPoint]
  ): Future[Vector[GetTxSpendingPrevOutResult]] = {
    val json = JsArray(prevouts.map { prev =>
      Json.obj("txid" -> prev.txIdBE.hex, "vout" -> prev.vout.toLong)
    })

    bitcoindCall[Vector[GetTxSpendingPrevOutResult]](
      "gettxspendingprevout",
      List(json)
    )
  }
}
