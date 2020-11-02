package org.bitcoins.core.api.keymanager

import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.wallet.keymanagement.{
  KeyManagerInitializeError,
  KeyManagerParams
}
import org.bitcoins.crypto.AesPassword
import scodec.bits.BitVector

trait KeyManagerCreateApi

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
trait BIP39KeyManagerCreateApi[T <: BIP39KeyManagerApi]
    extends KeyManagerCreateApi {

  /**
    * $initialize
    */
  final def initialize(
      aesPasswordOpt: Option[AesPassword],
      kmParams: KeyManagerParams,
      bip39PasswordOpt: Option[String]): Either[KeyManagerInitializeError, T] =
    initializeWithEntropy(aesPasswordOpt = aesPasswordOpt,
                          entropy = MnemonicCode.getEntropy256Bits,
                          bip39PasswordOpt = bip39PasswordOpt,
                          kmParams = kmParams)

  /**
    * $initializeWithEnt
    */
  def initializeWithEntropy(
      aesPasswordOpt: Option[AesPassword],
      entropy: BitVector,
      bip39PasswordOpt: Option[String],
      kmParams: KeyManagerParams): Either[KeyManagerInitializeError, T]

  /**
    * Helper method to initialize a [[KeyManagerApi KeyManager]] with a [[MnemonicCode MnemonicCode]]
    *
    * @param mnemonicCode
    * @param kmParams
    * @return
    */
  final def initializeWithMnemonic(
      aesPasswordOpt: Option[AesPassword],
      mnemonicCode: MnemonicCode,
      bip39PasswordOpt: Option[String],
      kmParams: KeyManagerParams): Either[KeyManagerInitializeError, T] = {
    val entropy = mnemonicCode.toEntropy
    initializeWithEntropy(aesPasswordOpt = aesPasswordOpt,
                          entropy = entropy,
                          bip39PasswordOpt = bip39PasswordOpt,
                          kmParams = kmParams)
  }
}
