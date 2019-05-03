package org.bitcoins.core.hd

case class HDCoin(purpose: HDPurpose, coinType: HDCoinType) extends BIP32Path {
  override def path: Vector[BIP32Node] =
    purpose.path :+ BIP32Node(coinType.toInt, hardened = true)

  def toAccount(index: Int): HDAccount = HDAccount(this, index)
}
