package org.bitcoins.keymanager

import java.nio.file.Path

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
  final def initialize(seedPath: Path): InitializeKeyManagerResult =
    initializeWithEntropy(entropy = MnemonicCode.getEntropy256Bits, seedPath)

  /**
    * $initializeWithEnt
    */
  def initializeWithEntropy(
      entropy: BitVector,
      seedPath: Path): InitializeKeyManagerResult

  // todo: scaladoc
  final def initializeWithMnemonic(
      mnemonicCode: MnemonicCode,
      seedPath: Path): InitializeKeyManagerResult = {
    val entropy = mnemonicCode.toEntropy
    initializeWithEntropy(entropy, seedPath)
  }
}
