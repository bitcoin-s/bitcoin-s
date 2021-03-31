package org.bitcoins.crypto

import scodec.bits.ByteVector

import javax.crypto.spec.PBEKeySpec
import javax.crypto.{SecretKey, SecretKeyFactory}

/** @define keyStretch Derives the provided value and salt to a secret key
  *                    using the PBKDF2 key derivation function
  *
  * Utilities related to the PBKDF2 key derivation function
  */
object PBKDF2 {

  /** This variant of PBKDF2 is used in
    * [[https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki#from-mnemonic-to-seed BIP39]]
    * and is generally speaking a recommended pseudo random function for PBKDF2. See for example
    * [[https://stackoverflow.com/questions/19348501/pbkdf2withhmacsha512-vs-pbkdf2withhmacsha1 this SO question]].
    */
  private val PSEUDO_RANDOM_FUNCTION = "PBKDF2WithHmacSHA512"

  private val secretKeyFactory =
    SecretKeyFactory.getInstance(PSEUDO_RANDOM_FUNCTION)

  /** $keyStretch */
  def withSha512(
      bytes: ByteVector,
      salt: ByteVector,
      iterationCount: Int,
      derivedKeyLength: Int): SecretKey = {

    val keySpec = new PBEKeySpec(
      bytes.toArray.map(_.toChar),
      salt.toArray,
      iterationCount,
      derivedKeyLength
    )

    secretKeyFactory.generateSecret(keySpec)
  }
}
