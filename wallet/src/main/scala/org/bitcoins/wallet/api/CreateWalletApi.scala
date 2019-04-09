package org.bitcoins.wallet.api

import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.db.DbConfig
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
  private def initializeInternal(
      chainParams: ChainParams,
      passphrase: String,
      dbConfig: Option[DbConfig])(
      implicit executionContext: ExecutionContext): Future[UnlockedWalletApi] =
    initializeWithEntropy(entropy = MnemonicCode.getEntropy256Bits,
                          chainParams = chainParams,
                          passphrase = passphrase,
                          dbConfig = dbConfig)

  /**
    * $initialize
    */
  final def initialize(chainParams: ChainParams, passphrase: String)(
      implicit executionContext: ExecutionContext): Future[UnlockedWalletApi] =
    initializeInternal(chainParams = chainParams,
                       passphrase = passphrase,
                       dbConfig = None)

  /**
    * $initialize
    */
  final def initialize(
      chainParams: ChainParams,
      passphrase: String,
      dbConfig: DbConfig)(
      implicit executionContext: ExecutionContext): Future[UnlockedWalletApi] =
    initializeInternal(chainParams = chainParams,
                       passphrase = passphrase,
                       dbConfig = Some(dbConfig))

  protected def initializeWithEntropy(
      entropy: BitVector,
      chainParams: ChainParams,
      passphrase: String,
      dbConfig: Option[DbConfig])(
      implicit executionContext: ExecutionContext): Future[UnlockedWalletApi]

  /**
    * $initializeWithEnt
    */
  final def initializeWithEntropy(
      entropy: BitVector,
      chainParams: ChainParams,
      passphrase: String
  )(implicit executionContext: ExecutionContext): Future[UnlockedWalletApi] =
    initializeWithEntropy(entropy = entropy,
                          chainParams = chainParams,
                          passphrase = passphrase,
                          dbConfig = None)

  /**
    * $initializeWithEnt
    */
  final def initializeWithEntropy(
      entropy: BitVector,
      chainParams: ChainParams,
      passphrase: String,
      dbConfig: DbConfig)(
      implicit executionContext: ExecutionContext): Future[UnlockedWalletApi] =
    initializeWithEntropy(entropy = entropy,
                          chainParams = chainParams,
                          passphrase = passphrase,
                          dbConfig = Some(dbConfig))
}
