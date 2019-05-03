package org.bitcoins.core.hd
import scala.util.Try

private[bitcoins] trait HDPath[T <: HDPath[T]] extends BIP32Path {

  /**
    * Increments the address index and returns the
    * new path that can be passed into a
    * [[org.bitcoins.core.crypto.ExtKey ExtKey]]
    */
  // TODO check out this cast
  def next: T =
    HDAddress(chain, account.index + 1).toPath.asInstanceOf[T]

  def account: HDAccount = address.account

  def purpose: HDPurpose = coin.purpose

  def coin: HDCoin = address.coin

  def chain: HDChain = address.chain

  def address: HDAddress

  override val path: Vector[BIP32Node] = address.path
}

object HDPath {

  /** Attempts to parse a string into a valid HD path */
  def fromString(string: String): Option[HDPath[_]] =
    Try(LegacyHDPath.fromString(string))
      .orElse(Try(SegWitHDPath.fromString(string)))
      .orElse(Try(NestedSegWitHDPath.fromString(string)))
      .toOption
}
