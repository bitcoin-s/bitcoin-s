package org.bitcoins.core.crypto.bip44
import org.bitcoins.core.crypto.bip32.{BIP32Child, BIP32Path}

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]]
  * change chain
  */
sealed abstract class BIP44Chain extends BIP32Path {
  override val children: Vector[BIP32Child] = account.children :+ BIP32Child(
    toInt,
    hardened = false)

  def coin: BIP44Coin

  def account: BIP44Account

  def chainType: BIP44ChainType

  def toInt: Int = chainType.index

}

object BIP44Chain {

  private case class BIP44ChainImpl(
      coin: BIP44Coin,
      chainType: BIP44ChainType,
      account: BIP44Account)
      extends BIP44Chain {}

  def apply(chainType: BIP44ChainType, account: BIP44Account): BIP44Chain =
    BIP44ChainImpl(account.coin, chainType, account)

}
