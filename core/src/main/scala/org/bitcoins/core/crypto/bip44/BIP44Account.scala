package org.bitcoins.core.crypto.bip44
import org.bitcoins.core.crypto.bip32.{BIP32Node, BIP32Path}

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#Account BIP44]]
  * account
  */
sealed abstract class BIP44Account extends BIP32Path {
  require(index >= 0, s"Account index ($index) must be positive!")

  override val path: Vector[BIP32Node] = {
    coin.path :+ BIP32Node(index, hardened = true)
  }

  def index: Int

  def coin: BIP44Coin

  def toChain(chainType: BIP44ChainType) =
    BIP44Chain(chainType = chainType, account = this)
}

object BIP44Account {
  private case class BIP44AccountImpl(coin: BIP44Coin, index: Int)
      extends BIP44Account

  def apply(coin: BIP44Coin, index: Int): BIP44Account =
    BIP44AccountImpl(coin, index)
}
