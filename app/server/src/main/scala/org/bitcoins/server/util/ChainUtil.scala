package org.bitcoins.server.util

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.Blockchain
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object ChainUtil {

  /** Calculates median time past for a given header, using a cached blockchain
    * to avoid redundant ancestor fetches.
    *
    * @param header
    *   the header to calculate median time for
    * @param blockchain
    *   cached blockchain that may contain this header and its ancestors
    * @param chain
    *   the chain API used to fetch a blockchain if the cached one cannot be
    *   used
    * @return
    *   `Some(mtp)` when median time past can be calculated, otherwise `None`
    *   when either: 1) the blockchain used for calculation does not contain
    *   `header`, or 2) the blockchain does not contain enough ancestor headers
    *   below `header` to compute median time past
    */
  private def getMedianTimePast(
      header: BlockHeaderDb,
      blockchain: Blockchain,
      chain: ChainApi
  )(implicit ec: ExecutionContext): Future[Option[Long]] = {
    Try {
      // Header not in cached chain, fetch its own
      blockchain.getMedianTimePast(header)
    } match {
      case scala.util.Success(mtpF) => Future.successful(Some(mtpF))
      case scala.util.Failure(_) =>
        chain
          .getBlockchainFrom(header)
          .map(b => b.map(_.getMedianTimePast))
    }
  }

  def getBlockHeaderResult(
      hashes: Vector[DoubleSha256DigestBE],
      chain: ChainApi
  )(implicit ec: ExecutionContext): Future[Vector[GetBlockHeaderResult]] = {
    val headersF: Future[Vector[Option[BlockHeaderDb]]] =
      chain.getHeaders(hashes)
    val bestHeaderF = chain.getBestBlockHeader()
    val bestHeightF = bestHeaderF.map(_.height)
    val bestBlockchainF: Future[Option[Blockchain]] =
      bestHeaderF.flatMap(bestHeader => chain.getBlockchainFrom(bestHeader))

    val headersWithConfsF: Future[Vector[Option[(BlockHeaderDb, Int)]]] = for {
      headers <- headersF
      bestHeight <- bestHeightF
    } yield {
      headers.map(hOpt => hOpt.map(h => (h, bestHeight - h.height)))
    }

    for {
      headersWithConfs <- headersWithConfsF
      cachedBlockchain <- bestBlockchainF
      results <- {
        cachedBlockchain match {
          case Some(blockchain) =>
            getBestBlockHeaderResults(headersWithConfs, blockchain, chain)
          case None =>
            Future.failed(
              new RuntimeException(
                "Could not fetch best block header or blockchain for median time past calculation"
              ))
        }

      }
    } yield {
      results
    }
  }

  private def getBestBlockHeaderResults(
      headersWithConfs: Vector[Option[(BlockHeaderDb, Int)]],
      blockchain: Blockchain,
      chain: ChainApi)(implicit
      ec: ExecutionContext): Future[Vector[GetBlockHeaderResult]] = {
    Future.traverse(headersWithConfs) {
      case None =>
        Future.failed(
          new RuntimeException(
            "Could not find block header or confirmations for the header"
          ))
      case Some((header, confs)) =>
        getMedianTimePast(header, blockchain, chain).map { medianTimePastOpt =>
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
            mediantime =
              medianTimePastOpt.map(UInt32.apply).getOrElse(header.time),
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
  }
}
