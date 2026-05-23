package org.bitcoins.server.util

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object ChainUtil {

  private val MedianTimePastSpan = 11

  private def getMedianTimePast(
      header: BlockHeaderDb,
      chain: ChainApi
  )(implicit ec: ExecutionContext): Future[UInt32] = {

    @tailrec
    def getNTopHeaders(
        n: Int,
        acc: Vector[Future[Option[BlockHeaderDb]]]
    ): Vector[Future[Option[BlockHeaderDb]]] = {
      if (n == 1) {
        acc
      } else {
        val prev: Future[Option[BlockHeaderDb]] = acc.last.flatMap {
          case None       => Future.successful(None)
          case Some(last) => chain.getHeader(last.previousBlockHashBE)
        }

        getNTopHeaders(n - 1, acc :+ prev)
      }
    }

    val topHeaders = getNTopHeaders(
      MedianTimePastSpan,
      Vector(Future.successful(Some(header)))
    )

    Future.sequence(topHeaders).map(_.flatten).map { headers =>
      val median = if (headers.length > 4) {
        Blockchain(headers).getMedianTimePast
      } else {
        val sortedTimes = headers.map(_.time.toLong).sorted
        sortedTimes(sortedTimes.length / 2)
      }

      UInt32(median)
    }
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
              mediantime = medianTimePast,
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
