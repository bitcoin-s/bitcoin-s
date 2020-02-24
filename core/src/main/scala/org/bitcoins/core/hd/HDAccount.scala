package org.bitcoins.core.hd

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#Account BIP44]],
  * [[https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki BIP84]]
  * and
  * [[https://github.com/bitcoin/bips/blob/master/bip-0049.mediawiki BIP49]]
  * account
  *
  * m / purpose' / coin_type' / account'
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

object HDAccount {

  def fromPath(path: BIP32Path): Option[HDAccount] = {

    HDCoin.fromPath(BIP32Path(path.path.init)).flatMap { coin =>
      val lastNode = path.path.last
      if (lastNode.hardened) {
        Some(HDAccount(coin, lastNode.index))
      } else {
        None
      }
    }
  }

  /** This method is meant to take in an arbitrary bip32 path and see
    * if it has the same account as the given account
    *
    * This is tricky as an account is defined as
    * m / purpose' / cointype' / account'
    *
    * whereas a bip32 path can be arbitrarily deep.
    *
    * We want to just check the first 4 elements of the path
    * and see if they are the same, which indicates we are in
    * the same account
    * */
  def isSameAccount(path: Vector[BIP32Node], account: HDAccount): Boolean = {
    if (account.path.length > path.length) {
      false
    } else {
      val zipped = path.zip(account.path)
      zipped.foldLeft(true) {
        case (past, (bip32Node, accountNode)) =>
          past && bip32Node == accountNode
      }
    }
  }

  def isSameAccount(bip32Path: BIP32Path, account: HDAccount): Boolean = {
    isSameAccount(bip32Path.toVector, account)
  }
}
