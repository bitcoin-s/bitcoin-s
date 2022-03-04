package org.bitcoins.core.hd

import org.bitcoins.crypto.StringFactory

import scala.util.{Failure, Success, Try}

trait HDPath extends BIP32Path {

  /** This type is to give a cleaner return
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

  /** Increments the address index and returns the
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

object HDPath extends StringFactory[HDPath] {

  /** Attempts to parse a string into a valid HD path */
  override def fromStringT(string: String): Try[HDPath] = {
    val path: BIP32Path = BIP32Path.fromString(string)

    val purpose = path.path.head.index
    if (purpose == LegacyHDPath.PURPOSE) {
      LegacyHDPath
        .fromStringT(string)
    } else if (purpose == SegWitHDPath.PURPOSE) {
      SegWitHDPath.fromStringT(string)
    } else if (purpose == NestedSegWitHDPath.PURPOSE) {
      NestedSegWitHDPath.fromStringT(string)
    } else {
      sys.error(s"Unknown purpose=$purpose")
    }
  }

  override def fromString(string: String): HDPath = {
    fromStringT(string) match {
      case Success(path) => path
      case Failure(exn)  => throw exn
    }
  }
}
