package org.bitcoins.core.crypto.bip44
import org.bitcoins.core.crypto.bip32.{BIP32Node, BIP32Path}

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#index BIP44]]
  * address index.
  */
sealed abstract class BIP44Address extends BIP32Path {
  require(index >= 0, s"Address index ($index) must be positive!")

  override val path: Vector[BIP32Node] = {
    chain.path :+ BIP32Node(index, hardened = false)
  }

  def coin: BIP44Coin
  def account: BIP44Account
  def chain: BIP44Chain
  def index: Int

  def toPath: BIP44Path = BIP44Path(this)
}

object BIP44Address {
  private case class BIP44AddressImpl(
      coin: BIP44Coin,
      account: BIP44Account,
      chain: BIP44Chain,
      index: Int)
      extends BIP44Address

  def apply(chain: BIP44Chain, index: Int): BIP44Address =
    BIP44AddressImpl(coin = chain.coin,
                     account = chain.account,
                     chain = chain,
                     index = index)
}
