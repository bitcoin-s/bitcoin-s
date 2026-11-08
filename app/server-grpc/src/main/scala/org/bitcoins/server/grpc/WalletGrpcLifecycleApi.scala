package org.bitcoins.server.grpc

import org.bitcoins.crypto.AesPassword
import org.bitcoins.keymanager.config.KeyManagerAppConfig

import scala.concurrent.Future

/** The pieces of wallet lifecycle / key management that
  * [[org.bitcoins.server.grpc.WalletGrpcRoutes]] needs but that cannot be
  * reached through `DLCNeutrinoHDWalletApi` alone.
  *
  * This lives in `server-grpc` so that module does not need to depend on the
  * wallet loader in `app/server`.
  */
trait WalletGrpcLifecycleApi {

  /** The key manager config of the currently loaded wallet. This is a `def` so
    * implementations can reflect a wallet swap.
    */
  def kmConf: KeyManagerAppConfig

  /** Loads (and switches to) the given wallet. */
  def loadWallet(
      walletNameOpt: Option[String],
      aesPasswordOpt: Option[AesPassword]): Future[Unit]
}
