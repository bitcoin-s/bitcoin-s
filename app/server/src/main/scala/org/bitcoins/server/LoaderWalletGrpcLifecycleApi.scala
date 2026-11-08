package org.bitcoins.server

import org.bitcoins.crypto.AesPassword
import org.bitcoins.keymanager.config.KeyManagerAppConfig
import org.bitcoins.server.grpc.WalletGrpcLifecycleApi

import scala.concurrent.{ExecutionContext, Future}

/** Backs the gRPC wallet lifecycle endpoints with a [[DLCWalletLoaderApi]], the
  * same thing the HTTP `loadwallet` route uses.
  */
class LoaderWalletGrpcLifecycleApi(loader: DLCWalletLoaderApi)(implicit
    ec: ExecutionContext)
    extends WalletGrpcLifecycleApi {

  override def kmConf: KeyManagerAppConfig = loader.conf.walletConf.kmConf

  override def loadWallet(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[Unit] =
    loader.load(walletNameOpt, aesPasswordOpt).map(_ => ())
}
