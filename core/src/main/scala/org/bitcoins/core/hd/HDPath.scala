package org.bitcoins.core.hd

import scala.util.Try

private[bitcoins] trait HDPath extends BIP32Path {

  /**
    * This type is to give a cleaner return
    * type of `next`.
    *
    * Consider:
    *
    * {{{
    * def next: this.type = ???
    *
    * val first: SegWitHDPath = ???
    * val second = first.next
    * // second is now:
    * // first.type (with underlying type org.bitcoins.core.hd.SegWitHDPath)
    * }}}
    *
    * {{{
    * def next: NextPath = ???
    *
    * // in SegWitHDPath
    * override type NextPath = SegWitHDPath
    *
    * val first: SegWitHDPath = ???
    * val second = first.next
    * // second is now:
    * // SegWitHDPath
    * }}}
    */
  protected type NextPath <: HDPath

  /**
    * Increments the address index and returns the
    * new path that can be passed into a
    * [[org.bitcoins.core.crypto.ExtKey ExtKey]]
    */
  def next: NextPath =
    HDAddress(chain, address.index + 1).toPath.asInstanceOf[NextPath]

  def account: HDAccount = address.account

  def purpose: HDPurpose = coin.purpose

  def coin: HDCoin = address.coin

  def chain: HDChain = address.chain

  def address: HDAddress

  override val path: Vector[BIP32Node] = address.path
}

object HDPath {

  /** Attempts to parse a string into a valid HD path */
  def fromString(string: String): Option[HDPath] =
    Try(LegacyHDPath.fromString(string))
      .orElse(Try(SegWitHDPath.fromString(string)))
      .orElse(Try(NestedSegWitHDPath.fromString(string)))
      .toOption
}
