package org.bitcoins.keymanager

import org.bitcoins.core.crypto.MnemonicCode
import scodec.bits.BitVector

/**
  * @define initialize
  *                    Initializes the wallet, generating a wallet seed.
  *                    This seed should be displayed to the user, so they
  *                    can write it down. They should also be prompted
  *                    to confirm at least parts of the code.
  * @define initializeWithEnt
  *                           Initializes the with a user-provided seed,
  *                           generating a wallet seed.
  *                           This seed should be displayed to the user, so they
  *                           can write it down. They should also be prompted
  *                           to confirm at least parts of the code.
  */
trait CreateKeyManagerApi {

  /**
    * $initialize
    */
  final def initialize(kmParams: KeyManagerParams): Either[
    InitializeKeyManagerError,
    KeyManager] =
    initializeWithEntropy(entropy = MnemonicCode.getEntropy256Bits, kmParams)

  /**
    * $initializeWithEnt
    */
  def initializeWithEntropy(
      entropy: BitVector,
      kmParams: KeyManagerParams): Either[InitializeKeyManagerError, KeyManager]

  // todo: scaladoc
  final def initializeWithMnemonic(
      mnemonicCode: MnemonicCode,
      kmParams: KeyManagerParams): Either[
    InitializeKeyManagerError,
    KeyManager] = {
    val entropy = mnemonicCode.toEntropy
    initializeWithEntropy(entropy = entropy, kmParams)
  }
}
