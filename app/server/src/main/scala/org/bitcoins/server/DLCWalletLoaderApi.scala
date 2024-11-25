package org.bitcoins.server

import org.apache.pekko.actor.ActorSystem
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.commons.ArgumentSource
import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.api.wallet.RescanHandlingApi
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.core.wallet.rescan.RescanState
import org.bitcoins.crypto.AesPassword
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.node.NodeCallbacks
import org.bitcoins.node.callback.NodeCallbackStreamManager
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.NodeStateDescriptorDAO
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.util.CallbackUtil
import org.bitcoins.wallet.WalletHolder
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future}

/** A trait used to help load a different load and discard the current wallet in
  * memory This trait encapsulates the heavy lifting done in the 'loadwallet'
  * RPC command
  */
sealed trait DLCWalletLoaderApi
    extends BitcoinSLogger
    with StartStopAsync[Unit] {

  override def start(): Future[Unit] = Future.unit
  def conf: BitcoinSAppConfig

  implicit protected def system: ActorSystem
  implicit private def ec: ExecutionContext = system.dispatcher

  def walletHolder: WalletHolder

  /** Determine if a wallet has been loaded */
  def isWalletLoaded: Boolean = walletHolder.isInitialized

  def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]
  ): Future[(WalletHolder, WalletAppConfig, DLCAppConfig)]

  private def loadWallet(
      chainQueryApi: ChainQueryApi,
      nodeApi: NodeApi,
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]
  )(implicit
      ec: ExecutionContext
  ): Future[(DLCNeutrinoHDWalletApi, WalletAppConfig, DLCAppConfig)] = {
    logger.info(
      s"Loading wallet with bitcoind backend, walletName=${walletNameOpt.getOrElse("DEFAULT")}"
    )
    val walletName =
      walletNameOpt.getOrElse(WalletAppConfig.DEFAULT_WALLET_NAME)

    for {
      (walletConfig, dlcConfig) <- updateWalletConfigs(
        walletName,
        aesPasswordOpt
      )
      _ <- walletConfig.start()
      _ <- dlcConfig.start()
      dlcWallet <- dlcConfig.createDLCWallet(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi
      )(walletConfig)
    } yield (dlcWallet, walletConfig, dlcConfig)
  }

  protected def updateWalletConfigs(
      walletName: String,
      aesPasswordOpt: Option[AesPassword]
  )(implicit ec: ExecutionContext): Future[(WalletAppConfig, DLCAppConfig)] = {
    val walletNameArgOpt = ArgumentSource.RpcArgument(walletName)
    val aesPasswordArgOpt = aesPasswordOpt match {
      case None =>
        Some(ArgumentSource.NoArgument)
      case Some(pw) =>
        Some(ArgumentSource.RpcArgument(pw))
    }
    val kmConfigF = Future.successful(
      conf.walletConf.kmConf.copy(
        walletNameOverride = Some(walletNameArgOpt),
        aesPasswordOverride = aesPasswordArgOpt
      )
    )

    (for {
      kmConfig <- kmConfigF

      // First thing start the key manager to be able to fail fast if the password is invalid
      _ <- kmConfig.start()

      walletConfig = conf.walletConf.copy(kmConfOpt = Some(kmConfig))
      dlcConfig = conf.dlcConf.copy(walletConfigOpt = Some(walletConfig))
    } yield (walletConfig, dlcConfig))
  }

  protected def updateWalletName(
      walletNameOpt: Option[String]
  )(implicit ec: ExecutionContext): Future[Unit] = {
    val nodeStateDAO: NodeStateDescriptorDAO =
      NodeStateDescriptorDAO()(ec, conf.nodeConf)
    nodeStateDAO.updateWalletName(walletNameOpt)
  }

  protected def restartRescanIfNeeded(
      wallet: RescanHandlingApi
  )(implicit ec: ExecutionContext): Future[RescanState] = {
    for {
      isRescanning <- wallet.isRescanning()
      res <-
        if (isRescanning)
          wallet.rescanNeutrinoWallet(
            startOpt = None,
            endOpt = None,
            addressBatchSize = wallet.discoveryBatchSize(),
            useCreationTime = true,
            force = true
          )
        else Future.successful(RescanState.RescanDone)
    } yield res
  }

  /** Store a rescan state for the wallet that is currently loaded This is
    * needed because we don't save rescan state anywhere else.
    */
  @volatile private var rescanStateOpt: Option[RescanState.RescanStarted] = None

  def setRescanState(rescanState: RescanState): Unit = {
    rescanState match {
      case RescanState.RescanAlreadyStarted =>
      // do nothing in this case, we don't need to keep these states around
      // don't overwrite the existing reference to RescanStarted
      case RescanState.RescanDone | RescanState.RescanNotNeeded =>
        // rescan is done, reset state
        rescanStateOpt = None
      case started: RescanState.RescanStarted =>
        if (rescanStateOpt.isEmpty) {
          // add callback to reset state when the rescan is done
          val resetStateCallbackF = started.entireRescanDoneF.map { _ =>
            rescanStateOpt = None
          }
          resetStateCallbackF.failed.foreach {
            case RescanState.RescanTerminatedEarly =>
              rescanStateOpt = None
            case exn: Throwable =>
              logger.error(
                s"Failed to reset rescanState in wallet loader. Resetting rescan state",
                exn
              )
              rescanStateOpt = None
          }
          rescanStateOpt = Some(started)
        } else {
          sys.error(
            s"Cannot run multiple rescans at the same time, got=$started have=$rescanStateOpt"
          )
        }
    }
  }

  protected def stopRescan()(implicit ec: ExecutionContext): Future[Unit] = {
    rescanStateOpt match {
      case Some(state) => state.stop().map(_ => ()) // stop the rescan
      case None        => Future.unit
    }
  }

  def isRescanStateEmpty: Boolean = rescanStateOpt.isEmpty

  def isRescanStateDefined: Boolean = !isRescanStateEmpty

  def clearRescanState(): Unit = {
    rescanStateOpt = None
    ()
  }

  protected def setDlcAppConfig(dlcAppConfig: DLCAppConfig): Unit = {
    currentDLCAppConfigOpt = Some(dlcAppConfig)
    ()
  }

  @volatile private var currentWalletAppConfigOpt: Option[WalletAppConfig] =
    None

  @volatile private var currentDLCAppConfigOpt: Option[DLCAppConfig] =
    None

  protected def stopOldWalletAppConfig(
      newWalletConfig: WalletAppConfig
  ): Future[Unit] = {
    currentWalletAppConfigOpt match {
      case Some(current) =>
        // stop the old config
        current
          .stop()
          .map(_ => {
            currentWalletAppConfigOpt = Some(newWalletConfig)
          })
      case None =>
        for {
          _ <- conf.walletConf.stop()
        } yield {
          currentWalletAppConfigOpt = Some(newWalletConfig)
        }
    }
  }

  protected def stopOldDLCAppConfig(
      newDlcConfig: DLCAppConfig
  ): Future[Unit] = {
    currentDLCAppConfigOpt match {
      case Some(current) =>
        // stop the old config
        current
          .stop()
          .map(_ => {
            currentDLCAppConfigOpt = Some(newDlcConfig)
          })
      case None =>
        for {
          _ <- conf.walletConf.stop()
        } yield {
          currentDLCAppConfigOpt = Some(newDlcConfig)
        }
    }
  }

  override def stop(): Future[Unit] = {
    val rescanStopF = rescanStateOpt match {
      case Some(rescanState) => rescanState.stop()
      case None              => Future.unit
    }

    val walletStopF = rescanStopF.flatMap { _ =>
      currentWalletAppConfigOpt match {
        case Some(w) => w.stop()
        case None    => Future.unit
      }
    }
    val dlcStopF = rescanStopF.flatMap { _ =>
      currentDLCAppConfigOpt match {
        case Some(d) => d.stop()
        case None    => Future.unit
      }
    }

    for {
      _ <- rescanStopF
      _ <- walletStopF
      _ <- dlcStopF
    } yield {
      logger.info(s"DLCWalletLoaderApi stopped")
      ()
    }
  }

  private def stopNodeCallbacks(nodeCallbacks: NodeCallbacks): Future[Unit] = {
    nodeCallbacks match {
      case stream: NodeCallbackStreamManager =>
        stream.stop()
      case _: NodeCallbacks =>
        Future.unit
    }
  }

  protected def loadHelper(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword],
      nodeConf: NodeAppConfig,
      chainQueryApi: ChainQueryApi,
      nodeApi: NodeApi,
      createCallbacks: DLCNeutrinoHDWalletApi => Future[
        NodeCallbackStreamManager])
      : Future[(WalletHolder, WalletAppConfig, DLCAppConfig)] = {
    logger.info(s"Beginning to load=$walletNameOpt")

    for {
      _ <- stopRescan()
      _ <- stopNodeCallbacks(nodeConf.callBacks)
      (dlcWallet, walletConfig, dlcConfig) <- loadWallet(
        chainQueryApi = chainQueryApi,
        nodeApi = nodeApi,
        walletNameOpt = walletNameOpt,
        aesPasswordOpt = aesPasswordOpt
      )
      _ <- stopOldWalletAppConfig(walletConfig)
      _ <- stopOldDLCAppConfig(dlcConfig)
      _ <- walletHolder.replaceWallet(dlcWallet)
      nodeCallbacks <- createCallbacks(dlcWallet)
      _ = nodeConf.replaceCallbacks(nodeCallbacks)
      _ <- updateWalletName(walletNameOpt)
      rescanState <- restartRescanIfNeeded(walletHolder.rescanHandling)
      _ = setRescanState(rescanState)
    } yield {
      logger.info(s"Done loading=$walletNameOpt")
      (walletHolder, walletConfig, dlcConfig)
    }
  }
}

case class DLCWalletNeutrinoBackendLoader(
    walletHolder: WalletHolder,
    chainQueryApi: ChainQueryApi,
    nodeApi: NodeApi
)(implicit
    override val conf: BitcoinSAppConfig,
    override val system: ActorSystem
) extends DLCWalletLoaderApi {

  implicit private val nodeConf: NodeAppConfig = conf.nodeConf

  override def isWalletLoaded: Boolean = walletHolder.isInitialized

  override def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]
  ): Future[(WalletHolder, WalletAppConfig, DLCAppConfig)] = {
    loadHelper(
      walletNameOpt = walletNameOpt,
      aesPasswordOpt = aesPasswordOpt,
      nodeConf = nodeConf,
      chainQueryApi = chainQueryApi,
      nodeApi = nodeApi,
      createCallbacks = CallbackUtil.createNeutrinoNodeCallbacksForWallet
    )
  }

}

case class DLCWalletBitcoindBackendLoader(
    walletHolder: WalletHolder,
    bitcoind: BitcoindRpcClient,
    nodeApi: NodeApi,
    feeProvider: FeeRateApi
)(implicit
    override val conf: BitcoinSAppConfig,
    override val system: ActorSystem
) extends DLCWalletLoaderApi {
  implicit private val nodeConf: NodeAppConfig = conf.nodeConf

  override def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]
  ): Future[(WalletHolder, WalletAppConfig, DLCAppConfig)] = {
    loadHelper(
      walletNameOpt = walletNameOpt,
      aesPasswordOpt = aesPasswordOpt,
      nodeConf = nodeConf,
      chainQueryApi = bitcoind,
      nodeApi = nodeApi,
      createCallbacks = CallbackUtil.createBitcoindNodeCallbacksForWallet
    )
  }
}
