package org.bitcoins.core.hd

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]]
  * change chain
  */
sealed abstract class HDChange extends BIP32Path {

  override val path: Vector[BIP32Node] = {
    account.path :+ BIP32Node(toInt, hardened = false)
  }

  def purpose: HDPurpose

  def coin: HDCoin

  def account: HDAccount

  def changeType: HDChangeType

  def toInt: Int = changeType.index

  /** Given a index, creates a HD address */
  def toHDAddress(index: Int): HDAddress = HDAddress(this, index = index)

}

object HDChange {

  private case class BIP44ChangeImpl(
      coin: HDCoin,
      changeType: HDChangeType,
      account: HDAccount,
      purpose: HDPurpose)
      extends HDChange

  def apply(changeType: HDChangeType, account: HDAccount): HDChange =
    BIP44ChangeImpl(account.coin, changeType, account, account.purpose)

}
