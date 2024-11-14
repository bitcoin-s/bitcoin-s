package org.bitcoins.core.hd

sealed abstract class TaprootHDPath extends HDPath {
  override protected type NextPath = TaprootHDPath
}

object TaprootHDPath extends HDPathFactory[TaprootHDPath] {
  override val PURPOSE: Int = 86

  private case class TaprootHDPathImpl(address: HDAddress) extends TaprootHDPath

  override def apply(
      coin: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): TaprootHDPath = {
    val address = assembleAddress(coin, accountIndex, chainType, addressIndex)
    TaprootHDPathImpl(address)
  }
}
