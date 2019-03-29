package org.bitcoins.core.crypto.bip44

/**
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]]
  */
sealed abstract class BIP44ChainType {
  def index: Int
}

object BIP44ChainType {

  /**
    * External chain is used for addresses that
    * are meant to be visible outside of the
    * wallet (e.g. for receiving payments).
    */
  final case object External extends BIP44ChainType {
    override val index: Int = 0
  }

  /**
    * Internal chain is used for addresses which
    * are not meant to be visible outside of the
    * wallet and is used for return transaction
    * change
    */
  final case object Change extends BIP44ChainType {
    override val index: Int = 1
  }

  def fromInt(int: Int): BIP44ChainType =
    int match {
      case External.index => BIP44ChainType.External
      case Change.index   => BIP44ChainType.Change
      case _: Int =>
        throw new IllegalArgumentException(
          s"$int is not a valid BIP44 change type!")
    }
}
