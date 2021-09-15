package org.bitcoins.core.hd

sealed abstract class TaprootHDPath extends HDPath {
  override protected type NextPath = TaprootHDPath
}

object TaprootHDPath extends HDPathFactory[TaprootHDPath] {

  /** The purpose constant from BIP86
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0086.mediawiki BIP86]]
    */
  override val PURPOSE: Int = 86

  private case class TaprootHDPathImpl(address: HDAddress) extends TaprootHDPath

  override def apply(
      coinType: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): TaprootHDPath = {

    val address =
      assembleAddress(coinType, accountIndex, chainType, addressIndex)
    TaprootHDPathImpl(address)
  }
}
