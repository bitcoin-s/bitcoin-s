package org.bitcoins.server

import akka.actor.ActorSystem
import grizzled.slf4j.Logging
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.core.util.StartStopAsync
import org.bitcoins.crypto.AesPassword
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.node.NodeCallbacks
import org.bitcoins.node.callback.NodeCallbackStreamManager
import org.bitcoins.node.models.NodeStateDescriptorDAO
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.util.CallbackUtil
import org.bitcoins.wallet.WalletHolder
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future}

/** A trait used to help load a different load and discard the current wallet in memory
  * This trait encapsulates the heavy lifting done in the 'loadwallet' RPC command
  */
sealed trait DLCWalletLoaderApi extends Logging with StartStopAsync[Unit] {

  override def start(): Future[Unit] = Future.unit
  protected def conf: BitcoinSAppConfig

  implicit protected def system: ActorSystem

  def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)]

  protected def loadWallet(
      walletHolder: WalletHolder,
      chainQueryApi: ChainQueryApi,
      nodeApi: NodeApi,
      feeProviderApi: FeeRateApi,
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword])(implicit
      ec: ExecutionContext): Future[
    (DLCNeutrinoHDWalletApi, WalletAppConfig, DLCAppConfig)] = {
    logger.info(
      s"Loading wallet with bitcoind backend, walletName=${walletNameOpt.getOrElse("DEFAULT")}")
    val stoppedCallbacksF = conf.nodeConf.callBacks match {
      case manager: NodeCallbackStreamManager =>
        manager.stop()
      case _: NodeCallbacks =>
        Future.unit
    }
    val walletName =
      walletNameOpt.getOrElse(WalletAppConfig.DEFAULT_WALLET_NAME)

    for {
      _ <- stoppedCallbacksF
      (walletConfig, dlcConfig) <- updateWalletConfigs(walletName,
                                                       Some(aesPasswordOpt))
      _ <- {
        if (walletHolder.isInitialized) {
          walletHolder
            .stop()
            .map(_ => ())
        } else {
          Future.unit
        }
      }
      _ <- walletConfig.start()
      _ <- dlcConfig.start()
      dlcWallet <- dlcConfig.createDLCWallet(
        nodeApi = nodeApi,
        chainQueryApi = chainQueryApi,
        feeRateApi = feeProviderApi
      )(walletConfig)
    } yield (dlcWallet, walletConfig, dlcConfig)
  }

  protected def updateWalletConfigs(
      walletName: String,
      aesPasswordOpt: Option[Option[AesPassword]])(implicit
      ec: ExecutionContext): Future[(WalletAppConfig, DLCAppConfig)] = {
    val kmConfigF = Future.successful(
      conf.walletConf.kmConf.copy(walletNameOverride = Some(walletName),
                                  aesPasswordOverride = aesPasswordOpt))

    (for {
      kmConfig <- kmConfigF
      _ = if (!kmConfig.seedExists())
        throw new RuntimeException(s"Wallet `${walletName}` does not exist")

      // First thing start the key manager to be able to fail fast if the password is invalid
      _ <- kmConfig.start()

      walletConfig = conf.walletConf.copy(kmConfOpt = Some(kmConfig))
      dlcConfig = conf.dlcConf.copy(walletConfigOpt = Some(walletConfig))
    } yield (walletConfig, dlcConfig))
  }

  protected def updateWalletName(walletNameOpt: Option[String])(implicit
      ec: ExecutionContext): Future[Unit] = {
    val nodeStateDAO: NodeStateDescriptorDAO =
      NodeStateDescriptorDAO()(ec, conf.nodeConf)
    nodeStateDAO.updateWalletName(walletNameOpt)
  }
}

case class DLCWalletNeutrinoBackendLoader(
    walletHolder: WalletHolder,
    chainQueryApi: ChainQueryApi,
    nodeApi: NodeApi,
    feeRateApi: FeeRateApi)(implicit
    override val conf: BitcoinSAppConfig,
    override val system: ActorSystem)
    extends DLCWalletLoaderApi {
  import system.dispatcher
  implicit private val nodeConf = conf.nodeConf

  private[this] var currentWalletAppConfigOpt: Option[WalletAppConfig] = None
  private[this] var currentDLCAppConfigOpt: Option[DLCAppConfig] = None

  override def stop(): Future[Unit] = {
    val walletStopF = currentWalletAppConfigOpt match {
      case Some(w) => w.stop()
      case None    => Future.unit
    }
    val dlcStopF = currentDLCAppConfigOpt match {
      case Some(d) => d.stop()
      case None    => Future.unit
    }

    for {
      _ <- walletStopF
      _ <- dlcStopF
    } yield ()
  }

  override def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)] = {

    for {
      (dlcWallet, walletConfig, dlcConfig) <- loadWallet(
        walletHolder = walletHolder,
        chainQueryApi = chainQueryApi,
        nodeApi = nodeApi,
        feeProviderApi = feeRateApi,
        walletNameOpt = walletNameOpt,
        aesPasswordOpt = aesPasswordOpt
      )
      _ <- stopOldWalletAppConfig(walletConfig)
      _ <- stopOldDLCAppConfig(dlcConfig)
      _ <- walletHolder.replaceWallet(dlcWallet)
      _ <- walletHolder.start()
      nodeCallbacks <-
        CallbackUtil.createNeutrinoNodeCallbacksForWallet(walletHolder)
      _ = nodeConf.replaceCallbacks(nodeCallbacks)
      _ <- updateWalletName(walletNameOpt)
    } yield (walletHolder, walletConfig, dlcConfig)
  }

  private def stopOldWalletAppConfig(
      newWalletConfig: WalletAppConfig): Future[Unit] = {
    currentWalletAppConfigOpt match {
      case Some(current) =>
        //stop the old config
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

  private def stopOldDLCAppConfig(newDlcConfig: DLCAppConfig): Future[Unit] = {
    currentDLCAppConfigOpt match {
      case Some(current) =>
        //stop the old config
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
}

case class DLCWalletBitcoindBackendLoader(
    walletHolder: WalletHolder,
    bitcoind: BitcoindRpcClient,
    nodeApi: NodeApi,
    feeProvider: FeeRateApi)(implicit
    override val conf: BitcoinSAppConfig,
    override val system: ActorSystem)
    extends DLCWalletLoaderApi {
  import system.dispatcher
  implicit private val nodeConf = conf.nodeConf

  private[this] var currentWalletAppConfigOpt: Option[WalletAppConfig] = None

  private[this] var currentDLCAppConfigOpt: Option[DLCAppConfig] = None

  override def stop(): Future[Unit] = {
    val walletStopF = currentWalletAppConfigOpt match {
      case Some(w) => w.stop()
      case None    => Future.unit
    }
    val dlcStopF = currentDLCAppConfigOpt match {
      case Some(d) => d.stop()
      case None    => Future.unit
    }

    for {
      _ <- walletStopF
      _ <- dlcStopF
    } yield ()
  }

  override def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)] = {
    for {
      (dlcWallet, walletConfig, dlcConfig) <- loadWallet(
        walletHolder = walletHolder,
        chainQueryApi = bitcoind,
        nodeApi = nodeApi,
        feeProviderApi = feeProvider,
        walletNameOpt = walletNameOpt,
        aesPasswordOpt = aesPasswordOpt)

      _ <- stopOldWalletAppConfig(walletConfig)
      _ <- stopOldDLCAppConfig(dlcConfig)
      nodeCallbacks <- CallbackUtil.createBitcoindNodeCallbacksForWallet(
        walletHolder)
      _ = nodeConf.addCallbacks(nodeCallbacks)
      _ <- walletHolder.replaceWallet(dlcWallet)
      _ <- walletHolder.start()
    } yield (walletHolder, walletConfig, dlcConfig)
  }

  private def stopOldWalletAppConfig(
      newWalletConfig: WalletAppConfig): Future[Unit] = {
    currentWalletAppConfigOpt match {
      case Some(current) =>
        //stop the old config
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

  private def stopOldDLCAppConfig(newDlcConfig: DLCAppConfig): Future[Unit] = {
    currentDLCAppConfigOpt match {
      case Some(current) =>
        //stop the old config
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
}
