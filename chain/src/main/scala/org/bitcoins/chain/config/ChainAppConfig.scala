package org.bitcoins.chain.config

import org.bitcoins.db._
import org.bitcoins.chain.models.BlockHeaderDAO
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Success
import scala.util.Failure

case object ChainAppConfig extends AppConfig {
  override val moduleConfigName: String = "chain.conf"

  def isDbInitialized()(implicit ec: ExecutionContext): Future[Boolean] = {
    val bhDAO = BlockHeaderDAO(this)
    val p = Promise[Boolean]()
    val isDefinedOptF = {
      bhDAO.read(chain.genesisBlock.blockHeader.hashBE).map(_.isDefined)
    }
    isDefinedOptF.onComplete {
      case Success(bool) => p.success(bool)
      case Failure(err) =>
        logger.info(s"Failed to init chain app err=${err.getMessage}")
        p.success(false)
    }

    p.future
  }
}
