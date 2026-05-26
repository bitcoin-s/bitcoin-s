package org.bitcoins.server.util

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

object ChainUtil {
  private def getMedianTimePast(
      header: BlockHeaderDb,
      chain: ChainApi
  )(implicit ec: ExecutionContext): Future[Long] = {
    val blockchainF = chain.getBlockchainFrom(header)
    blockchainF.map(_.get.getMedianTimePast)
  }

  def getBlockHeaderResult(
      hashes: Vector[DoubleSha256DigestBE],
      chain: ChainApi
  )(implicit ec: ExecutionContext): Future[Vector[GetBlockHeaderResult]] = {
    val headersF: Future[Vector[Option[BlockHeaderDb]]] =
      chain.getHeaders(hashes)
    val bestHeightF = chain.getBestBlockHeader().map(_.height)
    val headersWithConfsF: Future[Vector[Option[(BlockHeaderDb, Int)]]] = for {
      headers <- headersF
      bestHeight <- bestHeightF
    } yield {
      headers.map(hOpt => hOpt.map(h => (h, bestHeight - h.height)))
    }

    for {
      headersWithConfs <- headersWithConfsF
      results <- Future.traverse(headersWithConfs) {
        case None =>
          Future.failed(
            new RuntimeException(
              "Could not find block header or confirmations for the header"
            ))
        case Some((header, confs)) =>
          getMedianTimePast(header, chain).map { medianTimePast =>
            val chainworkStr = {
              val bytes = ByteVector(header.chainWork.toByteArray)
              val padded = if (bytes.length <= 32) {
                bytes.padLeft(32)
              } else bytes

              padded.toHex
            }
            GetBlockHeaderResult(
              hash = header.hashBE,
              confirmations = confs,
              height = header.height,
              version = header.version.toInt,
              versionHex = header.version,
              merkleroot = header.merkleRootHashBE,
              time = header.time,
              mediantime = UInt32(medianTimePast),
              nonce = header.nonce,
              bits = header.nBits,
              difficulty = None,
              chainwork = chainworkStr,
              previousblockhash = Some(header.previousBlockHashBE),
              nextblockhash = None,
              target = Some(NumberUtil.serializeTargetHex(header.target))
            )
          }
      }
    } yield {
      results
    }
  }
}
