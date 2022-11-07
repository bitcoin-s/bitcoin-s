package org.bitcoins.dlc.wallet

import akka.actor.ActorSystem
import com.typesafe.config.Config
import org.bitcoins.commons.config.{AppConfigFactoryBase, ConfigOps}
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.protocol.dlc.models.DLCState.{
  AdaptorSigComputationState,
  ClosedState
}
import org.bitcoins.core.protocol.dlc.models.{
  DLCState,
  EnumContractDescriptor,
  NumericContractDescriptor
}
import org.bitcoins.core.protocol.tlv.DLCSerializationVersion
import org.bitcoins.core.util.Mutable
import org.bitcoins.db.DatabaseDriver._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.internal.DLCDataManagement
import org.bitcoins.dlc.wallet.models.{
  DLCSetupDbState,
  OfferedDbState,
  SetupCompleteDLCDbState
}
import org.bitcoins.keymanager.config.KeyManagerAppConfig
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.TransactionDAO
import org.bitcoins.wallet.{Wallet, WalletLogger}

import java.nio.file._
import scala.concurrent.{ExecutionContext, Future}

/** Configuration for the Bitcoin-S wallet
  *
  * @param directory The data directory of the wallet
  * @param conf Optional sequence of configuration overrides
  */
case class DLCAppConfig(
    baseDatadir: Path,
    configOverrides: Vector[Config],
    walletConfigOpt: Option[WalletAppConfig] = None)(implicit
    val system: ActorSystem)
    extends DbAppConfig
    with DLCDbManagement
    with JdbcProfileComponent[DLCAppConfig] {
  implicit override val ec: ExecutionContext = system.dispatcher
  override protected[bitcoins] def moduleName: String = "dlc"
  override protected[bitcoins] type ConfigType = DLCAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Vector[Config]): DLCAppConfig =
    DLCAppConfig(baseDatadir, configs)

  override def appConfig: DLCAppConfig = this

  override def start(): Future[Unit] = {
    logger.debug(s"Initializing dlc setup")

    if (Files.notExists(datadir)) {
      Files.createDirectories(datadir)
    }

    //get migrations applied the last time the wallet was started
    val initMigrations = migrationsApplied()

    val numMigrations = {
      migrate()
    }

    val f = if (initMigrations != 0 && initMigrations <= 9) {
      //means we have an old wallet that we need to migrate

      serializationVersionMigration()
        .flatMap { _ =>
          deleteAlphaVersionDLCs()
        }
    } else {
      //the wallet is new enough where we cannot have any old
      //DLCs in the database with a broken contractId
      Future.unit
    }

    logger.info(
      s"Applied ${numMigrations.migrationsExecuted} to the dlc project. Started with initMigrations=$initMigrations")

    f
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
        val schema = PostgresUtil.getSchemaName(moduleName = moduleName,
                                                walletName = walletName)
        Some(schema)
      case SQLite =>
        None
    }
  }

  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit
      walletConf: WalletAppConfig): Future[DLCWallet] = {
    DLCAppConfig.createDLCWallet(nodeApi = nodeApi,
                                 chainQueryApi = chainQueryApi,
                                 feeRateApi = feeRateApi)(walletConf, this)
  }

  private val callbacks = new Mutable(DLCWalletCallbacks.empty)

  def walletCallbacks: DLCWalletCallbacks = callbacks.atomicGet

  def addCallbacks(newCallbacks: DLCWalletCallbacks): DLCWalletCallbacks = {
    callbacks.atomicUpdate(newCallbacks)(_ + _)
  }

  /** Delete alpha version DLCs, these are old protocol format DLCs that cannot be safely updated to the new protocol version of DLCs */
  private def deleteAlphaVersionDLCs(): Future[Unit] = {
    logger.info(s"Deleting alpha version DLCs")
    val dlcManagement = DLCDataManagement.fromDbAppConfig()(this, ec)
    val dlcDAO = dlcManagement.dlcDAO
    val alphaDLCsF =
      dlcDAO
        .findAll()
        .map(_.filter(_.serializationVersion == DLCSerializationVersion.Alpha))
    for {
      alphaDLCs <- alphaDLCsF
      _ <- Future.traverse(alphaDLCs) { dlc =>
        dlc.state match {
          case _: ClosedState | DLCState.Offered | DLCState.Accepted |
              _: AdaptorSigComputationState | DLCState.Signed =>
            logger.info(
              s"Deleting alpha version of a dlcId=${dlc.dlcId.hex} dlc=$dlc")
            dlcManagement.deleteByDLCId(dlc.dlcId)
          case DLCState.Broadcasted | DLCState.Confirmed =>
            sys.error(
              s"Cannot upgrade our DLC wallet as we have DLCs in progress using an ancient format of DLCs, dlcId=${dlc.dlcId.hex}")
        }
      }
    } yield ()
  }

  /** Correctly populates the serialization version for existing DLCs
    * in our wallet database
    */
  private def serializationVersionMigration(): Future[Unit] = {
    logger.info(s"Fixing serialization version for false positive Beta DLCs")
    val dlcManagement = DLCDataManagement.fromDbAppConfig()(this, ec)
    val dlcDAO = dlcManagement.dlcDAO
    //read all existing DLCs
    val allDlcsF = dlcDAO.findAll()

    //ugh, this is kinda nasty, idk how to make better though
    val walletAppConfig =
      WalletAppConfig(baseDatadir, configOverrides)
    val txDAO: TransactionDAO =
      TransactionDAO()(ec = ec, appConfig = walletAppConfig)
    //get the offers so we can figure out what the serialization version is
    val dlcDbContractInfoOfferF: Future[Vector[DLCSetupDbState]] = {
      for {
        allDlcs <- allDlcsF
        nestedOfferAndAccept = allDlcs.map { a =>
          val setupDbOptF =
            dlcManagement.getDLCFundingData(a.dlcId, txDAO = txDAO)

          setupDbOptF.foreach {
            case Some(_) => //happy path, do nothing
            case None =>
              logger.warn(s"Corrupted dlcId=${a.dlcId.hex} state=${a.state}, " +
                s"this is likely because of issue 4001 https://github.com/bitcoin-s/bitcoin-s/issues/4001 . " +
                s"This DLC will not have its contractId migrated to DLSerializationVersion.Beta")
          }
          setupDbOptF
        }
        offerAndAccepts <- Future.sequence(nestedOfferAndAccept)
      } yield {
        offerAndAccepts.flatten
      }
    }

    //now we need to insert the serialization type
    //into global_dlc_data
    val updatedDLCDbsF = for {
      dlcDbContractInfoOffer <- dlcDbContractInfoOfferF
    } yield setSerializationVersions(dlcDbContractInfoOffer)

    val updatedInDbF = updatedDLCDbsF.flatMap(dlcDAO.updateAll)

    updatedInDbF.map(_ => ())
  }

  /** Sets serialization versions on [[DLCDb]] based on the corresponding [[ContractInfo]] */
  private def setSerializationVersions(
      vec: Vector[DLCSetupDbState]): Vector[DLCDb] = {
    vec.map { case state: DLCSetupDbState =>
      val updatedDlcDb: DLCDb = state match {
        case acceptDbState: SetupCompleteDLCDbState =>
          val offer = acceptDbState.offer
          val dlcDb = acceptDbState.dlcDb
          offer.contractInfo.contractDescriptors.head match {
            case _: EnumContractDescriptor =>
              dlcDb.copy(tempContractId = offer.tempContractId,
                         serializationVersion = DLCSerializationVersion.Beta)
            case numeric: NumericContractDescriptor =>
              val version = numeric.outcomeValueFunc.serializationVersion
              if (version != dlcDb.serializationVersion) {
                logger.info(
                  s"Updating serializationVersion for dlcId=${dlcDb.dlcId.hex} old version=${dlcDb.serializationVersion} new version=${version}")
                dlcDb.copy(tempContractId = offer.tempContractId,
                           serializationVersion = version)
              } else {
                dlcDb
              }
          }
        case offerDbState: OfferedDbState =>
          //if we don't have an accept message, we can only calculate tempContractId
          val dlcDb = offerDbState.dlcDb
          val offer = offerDbState.offer

          offer.contractInfo.contractDescriptors.head match {
            case _: EnumContractDescriptor =>
              dlcDb.copy(tempContractId = offer.tempContractId,
                         serializationVersion = DLCSerializationVersion.Beta)
            case numeric: NumericContractDescriptor =>
              val version = numeric.outcomeValueFunc.serializationVersion
              if (version != dlcDb.serializationVersion) {
                logger.info(
                  s"Updating serializationVersion for dlcId=${dlcDb.dlcId.hex} old version=${dlcDb.serializationVersion} new version=${version}")
                dlcDb.copy(tempContractId = offer.tempContractId,
                           serializationVersion = version)
              } else {
                dlcDb
              }
          }
      }
      updatedDlcDb
    }
  }
}

object DLCAppConfig
    extends AppConfigFactoryBase[DLCAppConfig, ActorSystem]
    with WalletLogger {

  override val moduleName: String = "dlc"

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      system: ActorSystem): DLCAppConfig =
    DLCAppConfig(datadir, confs)

  /** Creates a wallet based on the given [[WalletAppConfig]] */
  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit
      walletConf: WalletAppConfig,
      dlcConf: DLCAppConfig): Future[DLCWallet] = {
    import dlcConf.ec
    val bip39PasswordOpt = walletConf.bip39PasswordOpt
    walletConf.hasWallet().flatMap { walletExists =>
      if (walletExists) {
        logger.info(s"Using pre-existing wallet")
        val wallet =
          DLCWallet(nodeApi, chainQueryApi, feeRateApi)
        Future.successful(wallet)
      } else {
        logger.info(s"Creating new wallet")
        val unInitializedWallet =
          DLCWallet(nodeApi, chainQueryApi, feeRateApi)

        Wallet
          .initialize(wallet = unInitializedWallet,
                      bip39PasswordOpt = bip39PasswordOpt)
          .map(_.asInstanceOf[DLCWallet])
      }
    }
  }
}
