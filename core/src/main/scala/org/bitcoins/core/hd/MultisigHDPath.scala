package org.bitcoins.core.hd

sealed abstract class MultisigHDPath extends HDPath {
  override protected type NextPath = MultisigHDPath
}

object MultisigHDPath extends HDPathFactory[MultisigHDPath] {

  /**
    * The purpose constant from BIP45
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0045.mediawiki BIP45]]
    */
  override val PURPOSE: Int = 45

  private case class MultisigHDPathImpl(address: HDAddress)
      extends MultisigHDPath

  override def apply(
      coinType: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): MultisigHDPath = {

    val address =
      assembleAddress(coinType, accountIndex, chainType, addressIndex)
    MultisigHDPathImpl(address)
  }
}
