package org.bitcoins.core.crypto.bip44
import org.bitcoins.core.crypto.bip32.{BIP32Node, BIP32Path}

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]]
  * change chain
  */
sealed abstract class BIP44Chain extends BIP32Path {
  override val path: Vector[BIP32Node] = {
    account.path :+ BIP32Node(toInt, hardened = false)
  }

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
