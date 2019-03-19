package org.bitcoins.core.crypto.bip32

import org.bitcoins.core.crypto.ExtKey
import org.bitcoins.core.number.UInt32

abstract class BIP32Path {
  def path: Vector[BIP32Node]

  override def toString: String =
    path
      .map {
        case BIP32Node(index, hardened) =>
          index.toString + (if (hardened) "'" else "")
      }
      .fold("m")((accum, curr) => accum + "/" + curr)

}

object BIP32Path {
  private case class BIP32PathImpl(path: Vector[BIP32Node]) extends BIP32Path

  /**
    * The empty BIP32 path "m", i.e. a path that does no
    * child key derivation
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki#the-key-tree BIP44]]
    *     section on key trees
    */
  val empty: BIP32Path = BIP32PathImpl(Vector.empty)

  def apply(path: Vector[BIP32Node]): BIP32Path = BIP32PathImpl(path)

  def apply(path: BIP32Node*): BIP32Path = BIP32Path(Vector(path: _*))

  /**
    * Parses a string representation of a BIP32 path. This is on the form
    * of
    *
    * {{{
    *   m/level/hardenedLevel'/...
    * }}}
    *
    * Where `level` is an integer index and hardenedLevel is an integer
    * index followed by a `'`. Different notation is used in BIP32, but this
    * is the most common way of writing down BIP32 paths.
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki BIP43]]
    *     and [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki BIP44]]
    *     for examples of this notation.
    */
  def fromString(string: String): BIP32Path = {
    val parts = string
      .split("/")
      .toVector
      // BIP32 path segments are written both with whitespace between (https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#examples)
      // and without (https://wiki.trezor.io/Standard_derivation_paths)
      .map(_.trim)

    val head +: rest = parts
    require(head == "m",
            """The first element in a BIP32 path string must be "m"""")

    val path = rest.map { str =>
      val (index: String, hardened: Boolean) =
        if (str.endsWith("'")) {
          (str.dropRight(1), true)
        } else {
          (str, false)
        }
      BIP32Node(index.toInt, hardened)
    }

    BIP32PathImpl(path)
  }
}

case class BIP32Node(index: Int, hardened: Boolean) {
  require(index >= 0, s"BIP32 node index must be positive! Got $index")

  /**
    * Converts this node to a BIP32 notation
    * unsigned 32 bit integer
    */
  def toUInt32: UInt32 =
    if (hardened) ExtKey.hardenedIdx + UInt32(index.toLong)
    else UInt32(index)
}
