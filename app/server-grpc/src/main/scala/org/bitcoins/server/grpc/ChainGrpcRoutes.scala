package org.bitcoins.server.grpc

import io.grpc.Status
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.commons.util.ChainUtil
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.crypto.DoubleSha256DigestBE
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class ChainGrpcRoutes(
    chainApi: ChainApi,
    network: BitcoinNetwork,
    startedTorConfigF: Future[Unit]
)(implicit ec: ExecutionContext)
    extends ChainRoutes {

  override def getInfo(in: GetInfoRequest): Future[GetInfoResponse] = {
    for {
      header <- chainApi.getBestBlockHeader()
      syncing <- chainApi.isSyncing()
      isIBD <- chainApi.isIBD()
    } yield {
      GetInfoResponse(
        network = network.name,
        blockHeight = header.height,
        blockHash = header.hashBE.hex,
        torStarted = startedTorConfigF.isCompleted,
        syncing = syncing,
        isInitialBlockDownload = isIBD
      )
    }
  }

  override def getBlockCount(
      in: GetBlockCountRequest): Future[GetBlockCountResponse] = {
    chainApi.getBlockCount().map(count => GetBlockCountResponse(count = count))
  }

  override def getFilterCount(
      in: GetFilterCountRequest): Future[GetFilterCountResponse] = {
    chainApi
      .getFilterCount()
      .map(count => GetFilterCountResponse(count = count))
  }

  override def getFilterHeaderCount(
      in: GetFilterHeaderCountRequest): Future[GetFilterHeaderCountResponse] = {
    chainApi
      .getFilterHeaderCount()
      .map(count => GetFilterHeaderCountResponse(count = count))
  }

  override def getBestBlockHash(
      in: GetBestBlockHashRequest): Future[GetBestBlockHashResponse] = {
    chainApi
      .getBestBlockHash()
      .map(hash => GetBestBlockHashResponse(hash = hash.hex))
  }

  override def getBlockHeader(
      in: GetBlockHeaderRequest): Future[GetBlockHeaderResponse] = {
    val hashEither =
      Try(DoubleSha256DigestBE(in.hash)).toEither.left.map(_.getMessage)

    hashEither match {
      case Left(err) =>
        Future.failed(
          Status.INVALID_ARGUMENT
            .withDescription(s"Invalid block hash: $err")
            .asRuntimeException())
      case Right(hash) =>
        val getBlockHeaderResultF = ChainUtil.getBlockHeaderResult(
          Vector(hash),
          chainApi
        )

        for {
          getBlockHeaderResult <- getBlockHeaderResultF
          result = getBlockHeaderResult.headOption
        } yield {
          result match {
            case None => GetBlockHeaderResponse(header = None)
            case Some(getBlockHeaderResult) =>
              val header = toBlockHeaderResult(getBlockHeaderResult)
              GetBlockHeaderResponse(header = Some(header))
          }
        }
    }
  }

  override def getMedianTimePast(
      in: GetMedianTimePastRequest): Future[GetMedianTimePastResponse] = {
    chainApi
      .getMedianTimePast()
      .map(mtp => GetMedianTimePastResponse(mediantimepast = mtp))
  }

  private def toBlockHeaderResult(
      header: GetBlockHeaderResult): BlockHeaderResult = {

    BlockHeaderResult(
      hash = header.hash.hex,
      confirmations = header.confirmations,
      height = header.height,
      version = header.version,
      versionHex = ByteVector.fromInt(header.version).toHex,
      merkleroot = header.merkleroot.hex,
      time = header.time.toLong.toInt,
      mediantime = header.mediantime.toInt,
      nonce = header.nonce.toLong.toInt,
      bits = header.bits.hex,
      chainwork = header.chainwork,
      previousblockhash = header.previousblockhash.map(_.hex),
      nextblockhash = None,
      target = header.target
    )
  }
}
