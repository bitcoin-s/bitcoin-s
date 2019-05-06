package org.bitcoins.wallet.api

import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.wallet.config.WalletAppConfig
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

  private def initializeInternal(appConfig: WalletAppConfig)(
      implicit executionContext: ExecutionContext): Future[
    InitializeWalletResult] =
    initializeWithEntropy(entropy = MnemonicCode.getEntropy256Bits, appConfig)

  /**
    * $initialize
    */
  final def initialize(appConfig: WalletAppConfig)(
      implicit executionContext: ExecutionContext): Future[
    InitializeWalletResult] =
    initializeInternal(appConfig)

  /**
    * $initializeWithEnt
    */
  def initializeWithEntropy(entropy: BitVector, appConfig: WalletAppConfig)(
      implicit executionContext: ExecutionContext): Future[
    InitializeWalletResult]

  // todo: scaladoc
  final def initializeWithMnemonic(
      mnemonicCode: MnemonicCode,
      appConfig: WalletAppConfig)(
      implicit executionContext: ExecutionContext): Future[
    InitializeWalletResult] = {
    val entropy = mnemonicCode.toEntropy
    initializeWithEntropy(entropy, appConfig)
  }
}
