package org.bitcoins.server.grpc

import io.grpc.Status
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.util.NumberUtil
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
        chainApi.getHeader(hash).flatMap {
          case None => Future.successful(GetBlockHeaderResponse(header = None))
          case Some(headerDb) =>
            chainApi.getBestBlockHeader().map { bestHeader =>
              val confirmations = bestHeader.height - headerDb.height + 1
              val header = toBlockHeaderResult(headerDb, confirmations)
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
      header: BlockHeaderDb,
      confirmations: Int): BlockHeaderResult = {
    val chainworkStr = {
      val bytes = ByteVector(header.chainWork.toByteArray)
      val padded = if (bytes.length <= 32) {
        bytes.padLeft(32)
      } else bytes

      padded.toHex
    }

    BlockHeaderResult(
      hash = header.hashBE.hex,
      confirmations = confirmations,
      height = header.height,
      version = header.version.toInt,
      versionHex = header.version.hex,
      merkleroot = header.merkleRootHashBE.hex,
      time = header.time.toLong.toInt,
      mediantime = header.time.toLong.toInt,
      nonce = header.nonce.toLong.toInt,
      bits = header.nBits.hex,
      chainwork = chainworkStr,
      previousblockhash = Some(header.previousBlockHashBE.hex),
      nextblockhash = None,
      target = Some(NumberUtil.serializeTargetHex(header.target))
    )
  }
}
