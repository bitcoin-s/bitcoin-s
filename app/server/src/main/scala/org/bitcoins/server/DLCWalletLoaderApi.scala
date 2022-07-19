package org.bitcoins.server

import grizzled.slf4j.Logging
import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.feeprovider.FeeRateApi
import org.bitcoins.core.api.node.NodeApi
import org.bitcoins.crypto.AesPassword
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.node.models.NodeStateDescriptorDAO
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.util.CallbackUtil
import org.bitcoins.wallet.WalletHolder
import org.bitcoins.wallet.config.WalletAppConfig

import scala.concurrent.{ExecutionContext, Future}

/** A trait used to help load a different load and discard the current wallet in memory
  * This trait encapsulates the heavy lifting done in the 'loadwallet' RPC command
  */
sealed trait DLCWalletLoaderApi extends Logging {

  protected def conf: BitcoinSAppConfig

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
    (AnyDLCHDWalletApi, WalletAppConfig, DLCAppConfig)] = {
    logger.info(
      s"Loading wallet with bitcoind backend, walletName=${walletNameOpt.getOrElse("DEFAULT")}")
    val walletName =
      walletNameOpt.getOrElse(WalletAppConfig.DEFAULT_WALLET_NAME)

    for {
      (walletConfig, dlcConfig) <- updateWalletConfigs(walletName,
                                                       Some(aesPasswordOpt))
        .recover { case _: Throwable => (conf.walletConf, conf.dlcConf) }
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
      )(walletConfig, ec)
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
    ec: ExecutionContext)
    extends DLCWalletLoaderApi {
  implicit private val nodeConf = conf.nodeConf

  override def load(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[
    (WalletHolder, WalletAppConfig, DLCAppConfig)] = {
    val nodeCallbacksF =
      CallbackUtil.createNeutrinoNodeCallbacksForWallet(walletHolder)
    val replacedNodeCallbacks = for {
      nodeCallbacks <- nodeCallbacksF
      _ = nodeConf.replaceCallbacks(nodeCallbacks)
    } yield ()

    for {
      _ <- replacedNodeCallbacks
      (dlcWallet, walletConfig, dlcConfig) <- loadWallet(
        walletHolder = walletHolder,
        chainQueryApi = chainQueryApi,
        nodeApi = nodeApi,
        feeProviderApi = feeRateApi,
        walletNameOpt = walletNameOpt,
        aesPasswordOpt = aesPasswordOpt
      )
      _ <- walletHolder.replaceWallet(dlcWallet)
      _ <- updateWalletName(walletNameOpt)
    } yield (walletHolder, walletConfig, dlcConfig)
  }
}

case class DLCWalletBitcoindBackendLoader(
    walletHolder: WalletHolder,
    bitcoind: BitcoindRpcClient,
    nodeApi: NodeApi,
    feeProvider: FeeRateApi)(implicit
    override val conf: BitcoinSAppConfig,
    ec: ExecutionContext)
    extends DLCWalletLoaderApi {
  implicit private val nodeConf = conf.nodeConf

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

      nodeCallbacks <- CallbackUtil.createBitcoindNodeCallbacksForWallet(
        walletHolder)
      _ = nodeConf.addCallbacks(nodeCallbacks)
      _ <- walletHolder.replaceWallet(dlcWallet)
    } yield (walletHolder, walletConfig, dlcConfig)
  }
}
