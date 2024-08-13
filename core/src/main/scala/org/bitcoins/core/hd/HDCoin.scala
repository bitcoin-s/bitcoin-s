package org.bitcoins.core.hd

import org.bitcoins.core.crypto.ExtKeyPubVersion.{
  LegacyMainNetPub,
  LegacyTestNet3Pub,
  NestedSegWitMainNetPub,
  NestedSegWitTestNet3Pub,
  SegWitMainNetPub,
  SegWitTestNet3Pub
}
import org.bitcoins.core.crypto.ExtKeyVersion
import org.bitcoins.core.crypto.ExtKeyVersion.{
  LegacyMainNetPriv,
  LegacyTestNet3Priv,
  NestedSegWitMainNetPriv,
  NestedSegWitTestNet3Priv,
  SegWitMainNetPriv,
  SegWitTestNet3Priv
}

/** Contains the path m / purpose' / coin_type' /
  *
  * @see
  *   https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#path-levels
  */
case class HDCoin(purpose: HDPurpose, coinType: HDCoinType) extends BIP32Path {

  override def path: Vector[BIP32Node] =
    purpose.path :+ BIP32Node(coinType.toInt, HardenedType.defaultOpt)

  def toAccount(index: Int): HDAccount = HDAccount(this, index)
}

object HDCoin {

  def fromPath(path: BIP32Path): Option[HDCoin] = {
    if (path.path.length == 2) {
      HDPurpose.fromNode(path.path.head).map { purpose =>
        val coinType = HDCoinType.fromInt(path.path.last.index)

        HDCoin(purpose, coinType)
      }
    } else {
      None
    }
  }

  def fromExtKeyVersion(version: ExtKeyVersion): HDCoin = {
    version match {
      case SegWitMainNetPriv | SegWitMainNetPub | SegWitTestNet3Priv |
          SegWitTestNet3Pub =>
        HDCoin(HDPurpose.SegWit, version.hdCoinType)
      case NestedSegWitMainNetPriv | NestedSegWitMainNetPub |
          NestedSegWitTestNet3Priv | NestedSegWitTestNet3Pub =>
        HDCoin(HDPurpose.NestedSegWit, version.hdCoinType)
      case LegacyMainNetPriv | LegacyMainNetPub | LegacyTestNet3Priv |
          LegacyTestNet3Pub =>
        HDCoin(HDPurpose.Legacy, version.hdCoinType)
    }
  }
}
