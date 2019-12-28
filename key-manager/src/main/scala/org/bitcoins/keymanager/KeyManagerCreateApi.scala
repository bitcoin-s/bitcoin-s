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
trait KeyManagerCreateApi[T <: KeyManager] {

  /**
    * $initialize
    */
  final def initialize(
      kmParams: KeyManagerParams): Either[KeyManagerInitializeError, T] =
    initializeWithEntropy(entropy = MnemonicCode.getEntropy256Bits, kmParams)

  /**
    * $initializeWithEnt
    */
  def initializeWithEntropy(
      entropy: BitVector,
      kmParams: KeyManagerParams): Either[KeyManagerInitializeError, T]

  /**
    * Helper method to initialize a [[KeyManagerCreate$ KeyManager]] with a [[MnemonicCode MnemonicCode]]
    *
    * @param mnemonicCode
    * @param kmParams
    * @return
    */
  final def initializeWithMnemonic(
      mnemonicCode: MnemonicCode,
      kmParams: KeyManagerParams): Either[KeyManagerInitializeError, T] = {
    val entropy = mnemonicCode.toEntropy
    initializeWithEntropy(entropy = entropy, kmParams)
  }
}
