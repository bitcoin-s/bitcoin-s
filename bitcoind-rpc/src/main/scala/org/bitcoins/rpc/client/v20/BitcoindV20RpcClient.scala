package org.bitcoins.rpc.client.v20

import akka.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.WalletFlag
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.chain.ChainQueryApi.FilterResponse
import org.bitcoins.core.api.chain.db.CompactFilterDb
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPrivateKey}
import org.bitcoins.rpc.client.common.{
  BitcoindRpcClient,
  BitcoindVersion,
  DescriptorRpc,
  PsbtRpc
}
import org.bitcoins.rpc.client.v19.V19BlockFilterRpc
import org.bitcoins.rpc.config.BitcoindInstance
import play.api.libs.json._

import scala.concurrent.Future
import scala.util.Try

/**
  * Class for creating a BitcoindV19 instance that can access RPCs
  */
class BitcoindV20RpcClient(override val instance: BitcoindInstance)(implicit
    actorSystem: ActorSystem)
    extends BitcoindRpcClient(instance)
    with DescriptorRpc
    with PsbtRpc
    with V19BlockFilterRpc
    with V20MultisigRpc
    with V20AssortedRpc {

  override def getFiltersBetweenHeights(
      startHeight: Int,
      endHeight: Int): Future[Vector[ChainQueryApi.FilterResponse]] = {
    val allHeights = startHeight.to(endHeight)

    def f(range: Vector[Int]): Future[Vector[FilterResponse]] = {
      val filterFs = range.map { height =>
        for {
          hash <- getBlockHash(height)
          filter <- getBlockFilter(hash, FilterType.Basic)
        } yield {
          FilterResponse(filter.filter, hash, height)
        }
      }
      Future.sequence(filterFs)
    }

    FutureUtil.batchExecute(elements = allHeights.toVector,
                            f = f,
                            init = Vector.empty,
                            batchSize = 25)
  }

  override def getFilterCount(): Future[Int] = getBlockCount

  override def getFilterHeaderCount(): Future[Int] = getBlockCount

  override def getFilter(
      hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] = {
    for {
      header <- getBlockHeader(hash)
      filter <- getBlockFilter(hash, FilterType.Basic)
    } yield Some(filter.filterDb(header.height))
  }

  override def getFiltersAtHeight(
      height: Int): Future[Vector[CompactFilterDb]] = {
    for {
      hash <- getBlockHash(height)
      filter <- getBlockFilter(hash, FilterType.Basic)
    } yield Vector(filter.filterDb(height))
  }

  override lazy val version: BitcoindVersion = BitcoindVersion.V20

  /**
    * $signRawTx
    *
    * This RPC call signs the raw transaction with keys found in
    * the Bitcoin Core wallet.
    */
  def signRawTransactionWithWallet(
      transaction: Transaction,
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter] =
        Vector.empty,
      sigHash: HashType = HashType.sigHashAll
  ): Future[SignRawTransactionResult] =
    bitcoindCall[SignRawTransactionResult]("signrawtransactionwithwallet",
                                           List(JsString(transaction.hex),
                                                Json.toJson(utxoDeps),
                                                Json.toJson(sigHash)))

  /**
    * $signRawTx
    *
    * This RPC call signs the raw transaction with keys provided
    * manually.
    */
  def signRawTransactionWithKey(
      transaction: Transaction,
      keys: Vector[ECPrivateKey],
      utxoDeps: Vector[RpcOpts.SignRawTransactionOutputParameter] =
        Vector.empty,
      sigHash: HashType = HashType.sigHashAll
  ): Future[SignRawTransactionResult] =
    bitcoindCall[SignRawTransactionResult]("signrawtransactionwithkey",
                                           List(JsString(transaction.hex),
                                                Json.toJson(keys),
                                                Json.toJson(utxoDeps),
                                                Json.toJson(sigHash)))

  /**
    * Change the state of the given wallet flag for a wallet.
    */
  def setWalletFlag(
      flag: WalletFlag,
      value: Boolean
  ): Future[SetWalletFlagResult] =
    bitcoindCall[SetWalletFlagResult](
      "setwalletflag",
      List(JsString(flag.toString), Json.toJson(value)))

  def getBalances: Future[GetBalancesResult] = {
    bitcoindCall[GetBalancesResult]("getbalances")
  }

}

object BitcoindV20RpcClient {

  /**
    * Creates an RPC client from the given instance.
    *
    * Behind the scenes, we create an actor system for
    * you. You can use `withActorSystem` if you want to
    * manually specify an actor system for the RPC client.
    */
  def apply(instance: BitcoindInstance): BitcoindV20RpcClient = {
    implicit val system =
      ActorSystem.create(BitcoindRpcClient.ActorSystemName)
    withActorSystem(instance)
  }

  /**
    * Creates an RPC client from the given instance,
    * together with the given actor system. This is for
    * advanced users, where you need fine grained control
    * over the RPC client.
    */
  def withActorSystem(instance: BitcoindInstance)(implicit
      system: ActorSystem): BitcoindV20RpcClient =
    new BitcoindV20RpcClient(instance)(system)

  def fromUnknownVersion(
      rpcClient: BitcoindRpcClient): Try[BitcoindV20RpcClient] =
    Try {
      new BitcoindV20RpcClient(rpcClient.instance)(rpcClient.system)
    }

}
