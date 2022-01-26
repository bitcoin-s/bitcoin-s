package org.bitcoins.dlc.wallet

import com.typesafe.config.Config
import org.bitcoins.commons.config.{AppConfigFactory, ConfigOps}
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.protocol.dlc.compute.DLCUtil
import org.bitcoins.core.protocol.tlv.DLCSerializationVersion
import org.bitcoins.core.util.Mutable
import org.bitcoins.db.DatabaseDriver._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.internal.DLCDataManagement
import org.bitcoins.dlc.wallet.models.{
  AcceptDbState,
  DLCSetupDbState,
  OfferedDbState
}
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
case class DLCAppConfig(baseDatadir: Path, configOverrides: Vector[Config])(
    implicit override val ec: ExecutionContext)
    extends DbAppConfig
    with DLCDbManagement
    with JdbcProfileComponent[DLCAppConfig] {
  override protected[bitcoins] def moduleName: String = "dlc"
  override protected[bitcoins] type ConfigType = DLCAppConfig

  override protected[bitcoins] def newConfigOfType(
      configs: Seq[Config]): DLCAppConfig =
    DLCAppConfig(baseDatadir, configs.toVector)

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

    val f = if (initMigrations != 0 && initMigrations <= 5) {
      //means we have an old wallet that we need to migrate
      logger.info(s"Running serialization version migration code")
      serializationVersionMigration()
    } else {
      //the wallet is new enough where we cannot have any old
      //DLCs in the database with a broken contractId
      Future.unit
    }

    logger.info(s"Applied $numMigrations to the dlc project")

    f
  }

  lazy val walletConf: WalletAppConfig =
    WalletAppConfig(baseDatadir, configOverrides)

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

  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit
      walletConf: WalletAppConfig,
      ec: ExecutionContext): Future[DLCWallet] = {
    DLCAppConfig.createDLCWallet(nodeApi = nodeApi,
                                 chainQueryApi = chainQueryApi,
                                 feeRateApi = feeRateApi)(walletConf, this, ec)
  }

  private val callbacks = new Mutable(DLCWalletCallbacks.empty)

  def walletCallbacks: DLCWalletCallbacks = callbacks.atomicGet

  def addCallbacks(newCallbacks: DLCWalletCallbacks): DLCWalletCallbacks = {
    callbacks.atomicUpdate(newCallbacks)(_ + _)
  }

  /** Correctly populates the serialization version for existing DLCs
    * in our wallet database
    */
  private def serializationVersionMigration(): Future[Unit] = {
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
        //only DLC with the alpha version need to be migrated
        alphaVersionDLCs = allDlcs.filter(
          _.serializationVersion == DLCSerializationVersion.Alpha)
        nestedOfferAndAccept = alphaVersionDLCs.map { a =>
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
        case acceptDbState: AcceptDbState =>
          val offer = acceptDbState.offer
          val acceptWithoutSigs = acceptDbState.acceptWithoutSigs
          val dlcDb = acceptDbState.dlcDb
          val contractId = DLCUtil.calcContractId(offer, acceptWithoutSigs)
          logger.info(
            s"Updating contractId for dlcId=${dlcDb.dlcId.hex} old contractId=${dlcDb.contractIdOpt
              .map(_.toHex)} new contractId=${contractId.toHex}")
          dlcDb.copy(tempContractId = offer.tempContractId,
                     contractIdOpt = Some(contractId),
                     serializationVersion = DLCSerializationVersion.Beta)
        case offerDbState: OfferedDbState =>
          //if we don't have an accept message, we can only calculate tempContractId
          val dlcDb = offerDbState.dlcDb
          val offer = offerDbState.offer
          logger.info(
            s"Updating tempContractId for dlcId=${dlcDb.dlcId.hex} old tempContractId=${dlcDb.tempContractId.hex} new contractId=${offer.tempContractId.hex}")
          dlcDb.copy(tempContractId = offer.tempContractId,
                     serializationVersion = DLCSerializationVersion.Beta)
      }
      updatedDlcDb
    }
  }
}

object DLCAppConfig extends AppConfigFactory[DLCAppConfig] with WalletLogger {

  override val moduleName: String = "dlc"

  /** Constructs a wallet configuration from the default Bitcoin-S
    * data directory and given list of configuration overrides.
    */
  override def fromDatadir(datadir: Path, confs: Vector[Config])(implicit
      ec: ExecutionContext): DLCAppConfig =
    DLCAppConfig(datadir, confs)

  /** Creates a wallet based on the given [[WalletAppConfig]] */
  def createDLCWallet(
      nodeApi: NodeApi,
      chainQueryApi: ChainQueryApi,
      feeRateApi: FeeRateApi)(implicit
      walletConf: WalletAppConfig,
      dlcConf: DLCAppConfig,
      ec: ExecutionContext): Future[DLCWallet] = {
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
