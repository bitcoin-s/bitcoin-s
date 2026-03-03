package org.bitcoins.dlc.wallet

import com.typesafe.config.Config
import org.apache.pekko.actor.ActorSystem
import org.bitcoins.commons.config.{AppConfigFactoryBase, ConfigOps}
import org.bitcoins.core.api.CallbackConfig
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.util.Mutable
import org.bitcoins.db.DatabaseDriver._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.callback.{
  DLCWalletCallbackStreamManager,
  DLCWalletCallbacks
}

import org.bitcoins.keymanager.config.KeyManagerAppConfig
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletLogger}

import java.nio.file._
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  *
  * @param directory
  *   The data directory of the wallet
  * @param conf
  *   Optional sequence of configuration overrides
  */
case class DLCAppConfig(
    baseDatadir: Path,
    configOverrides: Vector[Config],
    walletConfigOpt: Option[WalletAppConfig] = None
)(implicit val system: ActorSystem)
    extends DbAppConfig
    with DLCDbManagement
    with JdbcProfileComponent[DLCAppConfig]
    with CallbackConfig[DLCWalletCallbacks] {
  implicit override val ec: ExecutionContext = system.dispatcher
  override protected[bitcoins] def moduleName: String = "dlc"
  override protected[bitcoins] type ConfigType = DLCAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Vector[Config]
  ): DLCAppConfig =
    DLCAppConfig(baseDatadir, configs, walletConfigOpt = walletConfigOpt)

  override def appConfig: DLCAppConfig = this

  override def start(): Future[Unit] = {
    logger.debug(s"Initializing dlc setup")
    super.start().map { _ =>
      val numMigrations = migrate()

      logger.info(
        s"Applied ${numMigrations.migrationsExecuted} to the dlc project"
      )
    }

  }

  override def stop(): Future[Unit] = {
    val stopCallbacksF = callBacks match {
      case stream: DLCWalletCallbackStreamManager => stream.stop()
      case _: DLCWalletCallbacks =>
        Future.unit
    }
    stopCallbacksF.flatMap(_ => super.stop())
  }

  lazy val walletConf: WalletAppConfig =
    walletConfigOpt.getOrElse(WalletAppConfig(baseDatadir, configOverrides))

  lazy val walletName: String = walletConf.walletName

  override lazy val dbPath: Path = {
    val pathStrOpt =
      config.getStringOrNone(s"bitcoin-s.$moduleName.db.path")
    pathStrOpt match {
      case Some(pathStr) =>
        if (walletName == KeyManagerAppConfig.DEFAULT_WALLET_NAME) {
          Paths.get(pathStr)
        } else {
          Paths.get(pathStr).resolve(walletName)
        }
      case None =>
        sys.error(s"Could not find dbPath for $moduleName.db.path")
    }
  }

  override lazy val schemaName: Option[String] = {
    driver match {
      case PostgreSQL =>
        val schema = PostgresUtil.getSchemaName(
          moduleName = moduleName,
          walletName = walletName
        )
        Some(schema)
      case SQLite =>
        None
    }
  }

  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi
  )(implicit walletConf: WalletAppConfig): Future[DLCWallet] = {
    DLCAppConfig.createDLCWallet(
      nodeApi = nodeApi,
      chainQueryApi = chainQueryApi
    )(walletConf, this)
  }

  private val callbacks = new Mutable(DLCWalletCallbacks.empty)

  def walletCallbacks: DLCWalletCallbacks = callbacks.atomicGet

  override def addCallbacks(
      newCallbacks: DLCWalletCallbacks
  ): DLCWalletCallbacks = {
    callbacks.atomicUpdate(newCallbacks)(_ + _)
  }

  override lazy val callbackFactory: DLCWalletCallbacks.type =
    DLCWalletCallbacks

}

object DLCAppConfig
    extends AppConfigFactoryBase[DLCAppConfig, ActorSystem]
    with WalletLogger {

  override val moduleName: String = "dlc"

  /** Constructs a wallet configuration from the default Bitcoin-S data
    * directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      system: ActorSystem
  ): DLCAppConfig =
    DLCAppConfig(datadir, confs)

  /** Creates a wallet based on the given [[WalletAppConfig]] */
  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi
  )(implicit
      walletConf: WalletAppConfig,
      dlcConf: DLCAppConfig
  ): Future[DLCWallet] = {
    import dlcConf.ec
    walletConf.hasWallet().flatMap { walletExists =>
      if (walletExists) {
        logger.info(s"Using pre-existing wallet")
        val walletF = walletConf.createHDWallet(nodeApi, chainQueryApi)
        walletF.map(DLCWallet.apply)
      } else {
        logger.info(s"Creating new wallet")
        val unInitializedWallet =
          Wallet(nodeApi, chainQueryApi)

        Wallet
          .initialize(
            wallet = unInitializedWallet,
            accountHandling = unInitializedWallet.accountHandling
          )
          .map(DLCWallet.apply)
      }
    }
  }
}
