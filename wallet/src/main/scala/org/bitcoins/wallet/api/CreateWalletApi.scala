package org.bitcoins.wallet.api

import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.db.AppConfig
import scodec.bits.BitVector

import scala.concurrent.{ExecutionContext, Future}

/**
  * @define initialize
  *                    Initializes the wallet, generating a wallet seed.
  *                    This seed should be displayed to the user, so they
  *                    can write it down. They should also be prompted
  *                    to confirm at least parts of the code.
  *
  *
  * @define initializeWithEnt
  *                           Initializes the with a user-provided seed,
  *                           generating a wallet seed.
  *                           This seed should be displayed to the user, so they
  *                           can write it down. They should also be prompted
  *                           to confirm at least parts of the code.
  */
trait CreateWalletApi {

  /**
    */
  private def initializeInternal(appConfig: AppConfig)(
      implicit executionContext: ExecutionContext): Future[
    InitializeWalletResult] =
    initializeWithEntropy(entropy = MnemonicCode.getEntropy256Bits,
      appConfig)

  /**
    * $initialize
    */
  final def initialize(appConfig: AppConfig)(
      implicit executionContext: ExecutionContext): Future[
    InitializeWalletResult] =
    initializeInternal(appConfig)

  protected def initializeWithEntropy(
      entropy: BitVector,
      appConfig: AppConfig)(
      implicit executionContext: ExecutionContext): Future[
    InitializeWalletResult]
}
