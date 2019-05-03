package org.bitcoins.core.hd

case class HDPurpose(constant: Int) extends BIP32Path {
  override val path: Vector[BIP32Node] = Vector(
    BIP32Node(constant, hardened = true))
}

object HDPurposes {
  final val Legacy = HDPurpose(LegacyHDPath.PURPOSE)
  final val SegWit = HDPurpose(SegWitHDPath.PURPOSE)
  final val NestedSegWit = HDPurpose(NestedSegWitHDPath.PURPOSE)

  lazy val all: Vector[HDPurpose] = Vector(Legacy, SegWit, NestedSegWit)

  /** Tries to turn the provided integer into a HD purpose path segment */
  def fromConstant(i: Int): Option[HDPurpose] = all.find(_.constant == i)
}
