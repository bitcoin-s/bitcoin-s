package org.bitcoins.chain.config

import java.nio.file.Path

import com.typesafe.config.{Config, ConfigException}
import org.bitcoins.chain.ChainCallbacks
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.chain.pow.Pow
import org.bitcoins.core.api.chain.db.BlockHeaderDbHelper
import org.bitcoins.core.util.{FutureUtil, Mutable}
import org.bitcoins.db._

import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S chain verification module
  * @param directory The data directory of the module
  * @param confs Optional sequence of configuration overrides
  */
case class ChainAppConfig(
    private val directory: Path,
    private val confs: Config*)(implicit override val ec: ExecutionContext)
    extends AppConfig
    with ChainDbManagement
    with JdbcProfileComponent[ChainAppConfig] {

  override protected[bitcoins] def configOverrides: List[Config] = confs.toList
  override protected[bitcoins] def moduleName: String = "chain"
  override protected[bitcoins] type ConfigType = ChainAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): ChainAppConfig =
    ChainAppConfig(directory, configs: _*)
  protected[bitcoins] def baseDatadir: Path = directory

  override lazy val appConfig: ChainAppConfig = this

  private val callbacks = new Mutable(ChainCallbacks.empty)

  def chainCallbacks: ChainCallbacks = callbacks.atomicGet

  def addCallbacks(newCallbacks: ChainCallbacks): ChainCallbacks = {
    callbacks.atomicUpdate(newCallbacks)(_ + _)
  }

  /**
    * Checks whether or not the chain project is initialized by
    * trying to read the genesis block header from our block
    * header table
    */
  def isStarted(): Future[Boolean] = {
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
    */
  override def start(): Future[Unit] = {
    val numMigrations = migrate()

    logger.info(s"Applied ${numMigrations} to chain project")

    val isInitF = isStarted()
    isInitF.flatMap { isInit =>
      if (isInit) {
        FutureUtil.unit
      } else {
        val genesisHeader =
          BlockHeaderDbHelper.fromBlockHeader(
            height = 0,
            chainWork = Pow.getBlockProof(chain.genesisBlock.blockHeader),
            bh = chain.genesisBlock.blockHeader)

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
        config.getInt(s"$moduleName.neutrino.filter-header-batch-size.default")
    }
  }

  lazy val filterBatchSize: Int =
    config.getInt(s"${moduleName}.neutrino.filter-batch-size")

  lazy val forceRecalcChainWork: Boolean =
    config.getBooleanOrElse(s"$moduleName.force-recalc-chainwork",
                            default = false)
}

object ChainAppConfig extends AppConfigFactory[ChainAppConfig] {

  /** Constructs a chain verification configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): ChainAppConfig =
    ChainAppConfig(datadir, confs: _*)
}
