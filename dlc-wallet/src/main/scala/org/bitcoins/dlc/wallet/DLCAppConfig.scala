package org.bitcoins.dlc.wallet

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.keymanagement.KeyManagerInitializeError
import org.bitcoins.db.DatabaseDriver._
import org.bitcoins.db._
import org.bitcoins.keymanager.bip39.{BIP39KeyManager, BIP39LockedKeyManager}
import org.bitcoins.tor.Socks5ProxyParams
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.{Wallet, WalletLogger}

import java.net.{InetSocketAddress, URI}
import java.nio.file._
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  *
  * @param directory The data directory of the wallet
  * @param conf Optional sequence of configuration overrides
  */
case class DLCAppConfig(private val directory: Path, private val conf: Config*)(
    implicit override val ec: ExecutionContext)
    extends DbAppConfig
    with DLCDbManagement
    with JdbcProfileComponent[DLCAppConfig] {
  override protected[bitcoins] def configOverrides: List[Config] = conf.toList
  override protected[bitcoins] def moduleName: String = "dlc"
  override protected[bitcoins] type ConfigType = DLCAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): DLCAppConfig =
    DLCAppConfig(directory, configs: _*)

  protected[bitcoins] def baseDatadir: Path = directory

  override def appConfig: DLCAppConfig = this

  override def start(): Future[Unit] = {
    logger.debug(s"Initializing dlc setup")

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    val numMigrations = {
      migrate()
    }

    logger.info(s"Applied $numMigrations to the dlc project")

    FutureUtil.unit
  }

  lazy val walletConf: WalletAppConfig =
    WalletAppConfig(directory, conf: _*)

  lazy val walletNameOpt: Option[String] = walletConf.walletNameOpt

  override lazy val dbPath: Path = {
    val pathStrOpt =
      config.getStringOrNone(s"bitcoin-s.$moduleName.db.path")
    (pathStrOpt, walletNameOpt) match {
      case (Some(pathStr), Some(walletName)) =>
        Paths.get(pathStr).resolve(walletName)
      case (Some(pathStr), None) =>
        Paths.get(pathStr)
      case (None, Some(_)) | (None, None) =>
        sys.error(s"Could not find dbPath for $moduleName.db.path")
    }
  }

  override lazy val schemaName: Option[String] = {
    (driver, walletNameOpt) match {
      case (PostgreSQL, Some(walletName)) =>
        Some(s"${moduleName}_$walletName")
      case (PostgreSQL, None) =>
        Some(moduleName)
      case (SQLite, None) | (SQLite, Some(_)) =>
        None
    }
  }

  lazy val socks5ProxyParams: Option[Socks5ProxyParams] = {
    if (config.getBoolean("bitcoin-s.proxy.enabled")) {
      Some(
        Socks5ProxyParams(
          address = InetSocketAddress.createUnresolved(
            config.getString("bitcoin-s.proxy.host"),
            config.getInt("bitcoin-s.proxy.port")
          ),
          credentialsOpt = None,
          randomizeCredentials = true
        )
      )
    } else {
      None
    }
  }

  lazy val listenAddr: InetSocketAddress = {
    val str = config.getString(s"bitcoin-s.$moduleName.listen")
    val uri = new URI(str)
    InetSocketAddress.createUnresolved(uri.getHost, uri.getPort)
  }

  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit
      walletConf: WalletAppConfig,
      system: ActorSystem): Future[DLCWallet] = {
    DLCAppConfig.createDLCWallet(
      nodeApi = nodeApi,
      chainQueryApi = chainQueryApi,
      feeRateApi = feeRateApi)(walletConf, this, system)
  }
}

object DLCAppConfig extends AppConfigFactory[DLCAppConfig] with WalletLogger {

  override val moduleName: String = "dlc"

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): DLCAppConfig =
    DLCAppConfig(datadir, confs: _*)

  /** Creates a wallet based on the given [[WalletAppConfig]] */
  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit
      walletConf: WalletAppConfig,
      dlcConf: DLCAppConfig,
      system: ActorSystem): Future[DLCWallet] = {
    import system.dispatcher
    val aesPasswordOpt = walletConf.aesPasswordOpt
    val bip39PasswordOpt = walletConf.bip39PasswordOpt
    walletConf.hasWallet().flatMap { walletExists =>
      if (walletExists) {
        logger.info(s"Using pre-existing wallet")
        // TODO change me when we implement proper password handling
        BIP39LockedKeyManager.unlock(aesPasswordOpt,
                                     bip39PasswordOpt,
                                     walletConf.kmParams) match {
          case Right(km) =>
            val wallet =
              DLCWallet(km, nodeApi, chainQueryApi, feeRateApi, km.creationTime)
            Future.successful(wallet)
          case Left(err) =>
            sys.error(s"Error initializing key manager, err=${err}")
        }
      } else {
        logger.info(s"Initializing key manager")
        val keyManagerE: Either[KeyManagerInitializeError, BIP39KeyManager] =
          BIP39KeyManager.initialize(aesPasswordOpt = aesPasswordOpt,
                                     kmParams = walletConf.kmParams,
                                     bip39PasswordOpt = bip39PasswordOpt)

        val keyManager = keyManagerE match {
          case Right(keyManager) => keyManager
          case Left(err) =>
            sys.error(s"Error initializing key manager, err=${err}")
        }

        logger.info(s"Creating new wallet")
        val unInitializedWallet =
          DLCWallet(keyManager,
                    nodeApi,
                    chainQueryApi,
                    feeRateApi,
                    keyManager.creationTime)

        Wallet
          .initialize(wallet = unInitializedWallet,
                      bip39PasswordOpt = bip39PasswordOpt)
          .map(_.asInstanceOf[DLCWallet])
      }
    }
  }
}
