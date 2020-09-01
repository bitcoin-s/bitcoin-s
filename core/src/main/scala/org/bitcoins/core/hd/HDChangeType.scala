package org.bitcoins.core.hd

/**
  * Address chain (external vs. change) used by
  *
  * Format:
  * m / purpose' / coin_type' / account' / change
  *
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]],
  * [[https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki BIP84]]
  * and
  * [[https://github.com/bitcoin/bips/blob/master/bip-0049.mediawiki BIP49]]
  *
  * @see
  */
sealed abstract class HDChangeType {
  def index: Int
}

object HDChangeType {

  /**
    * External chain is used for addresses that
    * are meant to be visible outside of the
    * wallet (e.g. for receiving payments).
    */
  final case object External extends HDChangeType {
    override val index: Int = 0
  }

  /**
    * Internal chain is used for addresses which
    * are not meant to be visible outside of the
    * wallet and is used for return transaction
    * change
    */
  final case object Change extends HDChangeType {
    override val index: Int = 1
  }

  def fromInt(int: Int): HDChangeType =
    int match {
      case External.index => HDChangeType.External
      case Change.index   => HDChangeType.Change
      case _: Int =>
        throw new IllegalArgumentException(
          s"$int is not a valid BIP44 change type!")
    }
}
