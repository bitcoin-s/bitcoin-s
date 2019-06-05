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
  override protected val moduleName: String = "chain"
  override protected type ConfigType = ChainAppConfig
  override protected def newConfigOfType(
      configs: List[Config]): ChainAppConfig = ChainAppConfig(configs: _*)

  /**
    * Checks whether or not the chain project is initialized by
    * trying to read the genesis block header from our block
    * header table
    */
  def isInitialized()(implicit ec: ExecutionContext): Future[Boolean] = {
    val bhDAO =
      BlockHeaderDAO()(ec = implicitly[ExecutionContext], appConfig = this)
    val p = Promise[Boolean]()
    val isDefinedOptF = {
      bhDAO.read(chain.genesisBlock.blockHeader.hashBE).map(_.isDefined)
    }
    isDefinedOptF.onComplete {
      case Success(bool) =>
        logger.debug(s"Chain project is initialized")
        p.success(bool)
      case Failure(err) =>
        logger.info(s"Chain project is not initialized")
        p.success(false)
    }

    p.future
  }

  /** Initializes our chain project if it is needed
    * This creates the necessary tables for the chain project
    * and inserts preliminary data like the genesis block header
    * */
  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
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
        val blockHeaderDAO =
          BlockHeaderDAO()(ec = implicitly[ExecutionContext], appConfig = this)
        val bhCreatedF =
          createdF.flatMap(_ => blockHeaderDAO.create(genesisHeader))
        bhCreatedF.flatMap { _ =>
          logger.info(s"Inserted genesis block header into DB")
          FutureUtil.unit
        }
      }
    }
  }
}
