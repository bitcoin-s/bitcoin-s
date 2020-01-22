package org.bitcoins.chain.config

import java.nio.file.Path

import com.typesafe.config.Config
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.{BlockHeaderDAO, BlockHeaderDbHelper}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db._

import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S chain verification module
  * @param directory The data directory of the module
  * @param confs Optional sequence of configuration overrides
  */
case class ChainAppConfig(
    private val directory: Path,
    private val confs: Config*)
    extends AppConfig {
  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] val moduleName: String = "chain"
  override protected[bitcoins] type ConfigType = ChainAppConfig
  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): ChainAppConfig =
    ChainAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  /**
    * Checks whether or not the chain project is initialized by
    * trying to read the genesis block header from our block
    * header table
    */
  def isInitialized()(implicit ec: ExecutionContext): Future[Boolean] = {
    val bhDAO =
      BlockHeaderDAO()(ec = implicitly[ExecutionContext], appConfig = this)
    val isDefinedOptF = {
      bhDAO.read(chain.genesisBlock.blockHeader.hashBE).map(_.isDefined)
    }
    isDefinedOptF.foreach { _ =>
      logger.debug(s"Chain project is initialized")
    }
    isDefinedOptF.recover {
      case e: Throwable =>
        logger.info(s"Chain project is not initialized")
        false
    }
  }

  /** Initializes our chain project if it is needed
    * This creates the necessary tables for the chain project
    * and inserts preliminary data like the genesis block header
    * */
  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    val numMigrations = ChainDbManagement.migrate(this)

    logger.info(s"Applied ${numMigrations} to chain project")

    val isInitF = isInitialized()
    isInitF.flatMap { isInit =>
      if (isInit) {
        FutureUtil.unit
      } else {
        val genesisHeader =
          BlockHeaderDbHelper.fromBlockHeader(height = 0,
                                              bh =
                                                chain.genesisBlock.blockHeader)
        val blockHeaderDAO =
          BlockHeaderDAO()(ec = implicitly[ExecutionContext], appConfig = this)
        val bhCreatedF = blockHeaderDAO.create(genesisHeader)
        bhCreatedF.flatMap { _ =>
          logger.info(s"Inserted genesis block header into DB")
          FutureUtil.unit
        }
      }
    }
  }

  lazy val filterHeaderBatchSize: Int =
    config.getInt(s"${moduleName}.neutrino.filter-header-batch-size")

  lazy val filterBatchSize: Int =
    config.getInt(s"${moduleName}.neutrino.filter-batch-size")
}

object ChainAppConfig {

  /** Constructs a chain verification configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  def fromDefaultDatadir(confs: Config*): ChainAppConfig =
    ChainAppConfig(AppConfig.DEFAULT_BITCOIN_S_DATADIR, confs: _*)
}
