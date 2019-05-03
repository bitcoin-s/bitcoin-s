package org.bitcoins.core.hd

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#Account BIP44]],
  * [[https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki BIP84]]
  * and
  * [[https://github.com/bitcoin/bips/blob/master/bip-0049.mediawiki BIP49]]
  * account
  */
case class HDAccount(
    coin: HDCoin,
    index: Int
) extends BIP32Path {
  require(index >= 0, s"Account index ($index) must be positive!")

  override val path: Vector[BIP32Node] = {
    coin.path :+ BIP32Node(index, hardened = true)
  }

  def purpose: HDPurpose = coin.purpose

  def toChain(chainType: HDChainType): HDChain =
    HDChain(chainType = chainType, account = this)
}
