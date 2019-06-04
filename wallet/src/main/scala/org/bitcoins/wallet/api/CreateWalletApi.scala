package org.bitcoins.wallet.api

import org.bitcoins.core.crypto.MnemonicCode
import scodec.bits.BitVector

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.db.AppConfig

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

  private def initializeInternal()(
      implicit executionContext: ExecutionContext,
      appConfig: AppConfig): Future[InitializeWalletResult] =
    initializeWithEntropy(entropy = MnemonicCode.getEntropy256Bits)

  /**
    * $initialize
    */
  final def initialize()(
      implicit executionContext: ExecutionContext,
      appConfig: AppConfig): Future[InitializeWalletResult] =
    initializeInternal()

  /**
    * $initializeWithEnt
    */
  def initializeWithEntropy(entropy: BitVector)(
      implicit config: AppConfig,
      executionContext: ExecutionContext): Future[InitializeWalletResult]

  // todo: scaladoc
  final def initializeWithMnemonic(mnemonicCode: MnemonicCode)(
      implicit config: AppConfig,
      executionContext: ExecutionContext): Future[InitializeWalletResult] = {
    val entropy = mnemonicCode.toEntropy
    initializeWithEntropy(entropy)
  }
}
