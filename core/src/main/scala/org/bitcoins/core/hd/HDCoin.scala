package org.bitcoins.core.hd

case class HDCoin(purpose: HDPurpose, coinType: HDCoinType) extends BIP32Path {
  override def path: Vector[BIP32Node] =
    purpose.path :+ BIP32Node(coinType.toInt, hardened = true)

  def toAccount(index: Int): HDAccount = HDAccount(this, index)
}

object HDCoin {

  def fromPath(path: BIP32Path): Option[HDCoin] = {
    if (path.path.length == 2) {
      HDPurposes.fromNode(path.path.head).map { purpose =>
        val coinType = HDCoinType.fromInt(path.path.last.index)

        HDCoin(purpose, coinType)
      }
    } else {
      None
    }
  }
}
