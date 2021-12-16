package org.bitcoins.server.util

import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.crypto.DoubleSha256DigestBE
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

object ChainUtil {

  def getBlockHeaderResult(hash: DoubleSha256DigestBE, chain: ChainApi)(implicit
      ec: ExecutionContext): Future[GetBlockHeaderResult] = {
    val headerOptF = chain.getHeader(hash)
    val confsOptF = chain.getNumberOfConfirmations(hash)
    for {
      headerOpt <- headerOptF
      confsOpt <- confsOptF
    } yield {
      val zipped: Option[(BlockHeaderDb, Int)] = headerOpt.zip(confsOpt)
      zipped match {
        case None =>
          sys.error(
            s"Could not find block header hash=$hash or confirmations for the header ")
        case Some((header, confs)) =>
          val chainworkStr = {
            val bytes = ByteVector(header.chainWork.toByteArray)
            val padded = if (bytes.length <= 32) {
              bytes.padLeft(32)
            } else bytes

            padded.toHex
          }
          val result = GetBlockHeaderResult(
            hash = header.hashBE,
            confirmations = confs,
            height = header.height,
            version = header.version.toInt,
            versionHex = header.version,
            merkleroot = header.merkleRootHashBE,
            time = header.time,
            mediantime = header.time, // can't get this?
            nonce = header.nonce,
            bits = header.nBits,
            difficulty = BigDecimal(header.difficulty),
            chainwork = chainworkStr,
            previousblockhash = Some(header.previousBlockHashBE),
            nextblockhash = None
          )
          result
      }
    }
  }
}
