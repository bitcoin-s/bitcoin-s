package org.bitcoins.core.crypto

import org.bitcoins.crypto._
import scodec.bits.ByteVector

sealed abstract class BIP39Seed extends NetworkElement with MaskedToString {
  require(
    bytes.length <= MAX_SEED_LENGTH_BYTES && bytes.length >= MIN_SEED_LENGTH_BYTES,
    s"Seed must be between $MIN_SEED_LENGTH_BYTES and $MAX_SEED_LENGTH_BYTES bytes, got ${bytes.length}"
  )

  private def MIN_SEED_LENGTH_BYTES = 16
  private def MAX_SEED_LENGTH_BYTES = 64

  /** Generates an extended private key given a version */
  def toExtPrivateKey(keyVersion: ExtKeyPrivVersion): ExtPrivateKey =
    ExtPrivateKey.fromBIP39Seed(keyVersion, this)

  override def toStringSensitive: String = {
    s"BIP39Seed($hex)"
  }
}

/** @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki# BIP32]]
  */
object BIP39Seed extends Factory[BIP39Seed] {
  private case class BIP39SeedImpl(bytes: ByteVector) extends BIP39Seed

  /** Generates a
    * [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki BIP32]]
    * seed from a sequence of bytes. Must be between 16 and 64 bytes.
    */
  override def fromBytes(bytes: ByteVector): BIP39Seed =
    BIP39SeedImpl(bytes)

  val EMPTY_PASSWORD = ""

  private val ITERATION_COUNT = 2048
  private val DERIVED_KEY_LENGTH = 512

  /** Generates a
    * [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki BIP32]]
    * seed from a mnemonic code. An optional password can be supplied.
    * @param password
    *   Defaults to the empty string
    */
  def fromMnemonic(
      mnemonic: MnemonicCode,
      password: String = EMPTY_PASSWORD): BIP39Seed = {
    val salt = s"mnemonic$password"

    val words = mnemonic.mkString(" ")

    val encodedBytes = CryptoUtil.pbkdf2WithSha512(words,
                                                   salt,
                                                   ITERATION_COUNT,
                                                   DERIVED_KEY_LENGTH)

    BIP39Seed.fromBytes(encodedBytes)
  }

  /** Generates a
    * [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki BIP32]]
    * seed from a mnemonic code. An optional password can be supplied.
    */
  def fromMnemonic(
      mnemonic: MnemonicCode,
      passwordOpt: Option[String]): BIP39Seed = {
    passwordOpt match {
      case Some(pass) =>
        fromMnemonic(mnemonic, pass)
      case None =>
        fromMnemonic(mnemonic)
    }
  }
}
