package org.bitcoins.chain.config

import java.nio.file.Path

import com.typesafe.config.{Config, ConfigException}
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
    override val useLogbackConf: Boolean,
    private val confs: Config*)(implicit override val ec: ExecutionContext)
    extends AppConfig
    with ChainDbManagement
    with JdbcProfileComponent[ChainAppConfig] {

  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] def moduleName: String = "chain"
  override protected[bitcoins] type ConfigType = ChainAppConfig
  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): ChainAppConfig =
    ChainAppConfig(directory, useLogbackConf, configs: _*)
  protected[bitcoins] def baseDatadir: Path = directory

  override lazy val appConfig: ChainAppConfig = this

  /**
    * Checks whether or not the chain project is initialized by
    * trying to read the genesis block header from our block
    * header table
    */
  def isInitialized()(implicit ec: ExecutionContext): Future[Boolean] = {
    val bhDAO = BlockHeaderDAO()(ec, appConfig)
    val isDefinedOptF = {
      bhDAO.read(chain.genesisBlock.blockHeader.hashBE).map(_.isDefined)
    }
    isDefinedOptF.foreach { _ =>
      logger.debug(s"Chain project is initialized")
    }
    isDefinedOptF.recover {
      case _: Throwable =>
        logger.info(s"Chain project is not initialized")
        false
    }
  }

  /** Initializes our chain project if it is needed
    * This creates the necessary tables for the chain project
    * and inserts preliminary data like the genesis block header
    * */
  override def initialize()(implicit ec: ExecutionContext): Future[Unit] = {
    val numMigrations = migrate()

    logger.info(s"Applied ${numMigrations} to chain project")

    val isInitF = isInitialized()
    isInitF.flatMap { isInit =>
      if (isInit) {
        FutureUtil.unit
      } else {
        val genesisHeader =
          BlockHeaderDbHelper.fromBlockHeader(height = 0,
                                              chainWork = BigInt(0),
                                              bh =
                                                chain.genesisBlock.blockHeader)
        val blockHeaderDAO = BlockHeaderDAO()(ec, appConfig)
        val bhCreatedF = blockHeaderDAO.create(genesisHeader)
        bhCreatedF.flatMap { _ =>
          logger.info(s"Inserted genesis block header into DB")
          FutureUtil.unit
        }
      }
    }
  }

  lazy val filterHeaderBatchSize: Int = {
    // try by network, if that fails, try general
    try {
      config.getInt(
        s"$moduleName.neutrino.filter-header-batch-size.${chain.network.chainParams.networkId}")
    } catch {
      case _: ConfigException.Missing | _: ConfigException.WrongType =>
        config.getInt(s"$moduleName.neutrino.filter-header-batch-size")
    }
  }

  lazy val filterBatchSize: Int =
    config.getInt(s"${moduleName}.neutrino.filter-batch-size")
}

object ChainAppConfig extends AppConfigFactory[ChainAppConfig] {

  /** Constructs a chain verification configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(
      datadir: Path,
      useLogbackConf: Boolean,
      confs: Vector[Config])(implicit ec: ExecutionContext): ChainAppConfig =
    ChainAppConfig(datadir, useLogbackConf, confs: _*)
}
