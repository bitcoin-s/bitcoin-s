package org.bitcoins.chain.config

import org.bitcoins.chain.db.ChainDbConfig
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.db.AppConfig
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}

case class ChainAppConfig(dbConfig: ChainDbConfig) extends AppConfig[ChainDbConfig] {

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
