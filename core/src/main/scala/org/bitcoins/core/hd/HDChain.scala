package org.bitcoins.core.hd

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]]
  * change chain
  */
sealed abstract class HDChain extends BIP32Path {
  override val path: Vector[BIP32Node] = {
    account.path :+ BIP32Node(toInt, hardened = false)
  }

  def purpose: HDPurpose

  def coin: HDCoin

  def account: HDAccount

  def chainType: HDChainType

  def toInt: Int = chainType.index

  /** Given a index, creates a HD address */
  def toHDAddress(index: Int): HDAddress = HDAddress(this, index = index)

}

object HDChain {

  private case class BIP44ChainImpl(
      coin: HDCoin,
      chainType: HDChainType,
      account: HDAccount,
      purpose: HDPurpose)
      extends HDChain

  def apply(chainType: HDChainType, account: HDAccount): HDChain =
    BIP44ChainImpl(account.coin, chainType, account, account.purpose)

}
