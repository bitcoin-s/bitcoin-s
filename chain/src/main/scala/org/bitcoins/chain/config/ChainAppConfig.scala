package org.bitcoins.chain.config

import com.typesafe.config.Config
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.db._
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDbHelper}
import org.bitcoins.core.util.FutureUtil

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.Promise
import scala.util.Success
import scala.util.Failure

case class ChainAppConfig(val confs: Config*) extends AppConfig {
  override protected val configOverrides: List[Config] = confs.toList
  override protected val moduleConfigName: String = "chain.conf"
  override protected type ConfigType = ChainAppConfig
  override protected def newConfigOfType(
      configs: List[Config]): ChainAppConfig = ChainAppConfig(configs: _*)

  def isInitialized()(implicit ec: ExecutionContext): Future[Boolean] = {
    val bhDAO = BlockHeaderDAO(this)
    val p = Promise[Boolean]()
    val isDefinedOptF = {
      bhDAO.read(chain.genesisBlock.blockHeader.hashBE).map(_.isDefined)
    }
    isDefinedOptF.onComplete {
      case Success(bool) =>
        logger.info(s"Chain project is initialized")
        p.success(bool)
      case Failure(err) =>
        logger.info(s"Failed to init chain app err=${err.getMessage}")
        p.success(false)
    }

    p.future
  }

  /** Initializes our chain project if it is needed
    * This creates the necessary tables for the chain project
    * and inserts preliminary data like the genesis block header
    * */
  def initialize(implicit ec: ExecutionContext): Future[Unit] = {
    val blockHeaderDAO = BlockHeaderDAO(this)
    val isInitF = isInitialized()
    isInitF.flatMap { isInit =>
      if (isInit) {
        FutureUtil.unit
      } else {
        val createdF = ChainDbManagement.createAll()(this, ec)
        val genesisHeader =
          BlockHeaderDbHelper.fromBlockHeader(height = 0,
                                              bh =
                                                chain.genesisBlock.blockHeader)
        val bhCreatedF =
          createdF.flatMap(_ => blockHeaderDAO.create(genesisHeader))
        bhCreatedF.flatMap(_ => FutureUtil.unit)
      }
    }
  }
}
