package org.bitcoins.wallet.util

import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.util.BlockHashWithConfs
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.{ExecutionContext, Future}

object WalletUtil {

  def getBlockHashWithConfs(
      chainQueryApi: ChainQueryApi,
      blockHashOpt: Option[DoubleSha256DigestBE])(implicit
      ec: ExecutionContext): Future[Option[BlockHashWithConfs]] = {
    blockHashOpt match {
      case Some(blockHash) =>
        chainQueryApi
          .getNumberOfConfirmations(blockHash)
          .map(confsOpt => Some(BlockHashWithConfs(blockHash, confsOpt)))
      case None => Future.successful(None)
    }
  }

  def getBlockHashWithConfs(
      chainQueryApi: ChainQueryApi,
      blockHash: DoubleSha256DigestBE)(implicit
      ec: ExecutionContext): Future[Option[BlockHashWithConfs]] = {
    getBlockHashWithConfs(chainQueryApi, Some(blockHash))
  }
}
