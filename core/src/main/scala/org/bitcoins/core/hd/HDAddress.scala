package org.bitcoins.core.hd

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#index BIP44]]
  * address index.
  */
sealed abstract class HDAddress extends BIP32Path {
  require(index >= 0, s"Address index ($index) must be positive!")

  override val path: Vector[BIP32Node] = {
    change.path :+ BIP32Node(index, hardened = false)
  }

  def purpose: HDPurpose
  def coin: HDCoin
  def account: HDAccount
  def change: HDChange
  def index: Int

  def toPath: HDPath =
    purpose match {
      case HDPurposes.Legacy       => LegacyHDPath(this)
      case HDPurposes.SegWit       => SegWitHDPath(this)
      case HDPurposes.NestedSegWit => NestedSegWitHDPath(this)
      case unknown: HDPurpose =>
        throw new IllegalArgumentException(s"Unknown HD purpose $unknown")
    }
}

object HDAddress {

  private case class HDAddressImpl(
      coin: HDCoin,
      account: HDAccount,
      change: HDChange,
      index: Int,
      purpose: HDPurpose)
      extends HDAddress

  def apply(change: HDChange, index: Int): HDAddress =
    HDAddressImpl(coin = change.coin,
                  account = change.account,
                  change = change,
                  index = index,
                  purpose = change.purpose)

}
