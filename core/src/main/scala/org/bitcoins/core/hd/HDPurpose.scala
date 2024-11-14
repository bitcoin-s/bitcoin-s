package org.bitcoins.core.hd

import org.bitcoins.crypto.StringFactory

/** This is a field that is used in conjunction with BIP44 to indicate what the
  * purpose of this [[org.bitcoins.core.crypto.ExtKey ExtKey]] is.
  *
  * This has been used for deploying keychains that are compatible with raw
  * segwit, p2sh wrapped segwit, and raw scripts.
  *
  * Format: m / purpose'
  *
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki BIP43]]
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#Purpose BIP44]]
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0045.mediawiki#Purpose BIP45]]
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki BIP84]]
  * @see
  *   [[https://github.com/bitcoin/bips/blob/master/bip-0049.mediawiki BIP49]]
  */
case class HDPurpose(constant: Int) extends BIP32Path {

  override val path: Vector[BIP32Node] = Vector(
    BIP32Node(constant, HardenedType.defaultOpt))
}

object HDPurpose extends StringFactory[HDPurpose] {
  final val Legacy = HDPurpose(LegacyHDPath.PURPOSE)
  final val Multisig = HDPurpose(MultisigHDPath.PURPOSE)
  final val SegWit = HDPurpose(SegWitHDPath.PURPOSE)
  final val NestedSegWit = HDPurpose(NestedSegWitHDPath.PURPOSE)
  final val Taproot = HDPurpose(TaprootHDPath.PURPOSE)

  final val default: HDPurpose = SegWit

  lazy val singleSigPurposes: Vector[HDPurpose] =
    Vector(Legacy, SegWit, NestedSegWit, Taproot)

  lazy val all: Vector[HDPurpose] =
    Vector(Legacy, Multisig, SegWit, NestedSegWit, Taproot)

  /** Tries to turn the provided integer into a HD purpose path segment */
  def fromConstant(i: Int): Option[HDPurpose] = all.find(_.constant == i)

  def fromNode(node: BIP32Node): Option[HDPurpose] = {
    require(node.hardened,
            s"Cannot construct HDPurpose from un-hardened node: $node")
    fromConstant(node.index)
  }

  override def fromString(string: String): HDPurpose = {
    val node = BIP32Node.fromString(string)
    fromNode(node).getOrElse {
      sys.error(s"Cannot create HDPurpose from string=$string")
    }
  }
}
