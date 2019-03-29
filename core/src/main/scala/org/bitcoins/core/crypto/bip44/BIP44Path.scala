package org.bitcoins.core.crypto.bip44

import org.bitcoins.core.crypto.bip32.{BIP32Node, BIP32Path}

import scala.util.Try

sealed abstract class BIP44Path extends BIP32Path {

  def account: BIP44Account = address.account

  def coin: BIP44Coin = address.coin

  def chain: BIP44Chain = address.chain

  def address: BIP44Address

  override val path: Vector[BIP32Node] = address.path

}

object BIP44Path {

  /**
    * The purpose constant from BIP44
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#purpose BIP44]]
    */
  private val PURPOSE: Int = 44

  /**
    * The index of the coin segement of a BIP44 path
    */
  val COIN_INDEX: Int = 1

  /**
    * The index of the account segement of a BIP44 path
    */
  val ACCOUNT_INDEX: Int = 2

  /**
    * The index of the chain segement of a BIP44 path
    */
  val CHAIN_INDEX: Int = 3

  /**
    * The index of the address segement of a BIP44 path
    */
  val ADDRESS_INDEX: Int = 4

  val purposeChild: BIP32Node = BIP32Node(PURPOSE, hardened = true)

  private case class BIP44PathImpl(address: BIP44Address) extends BIP44Path

  def apply(address: BIP44Address): BIP44Path = {
    BIP44Path(coin = address.coin,
              accountIndex = address.account.index,
              chainType = address.chain.chainType,
              addressIndex = address.index)
  }

  /**
    * Tries to generate a BIP44 path from the given path segments
    */
  def apply(children: Vector[BIP32Node]): Try[BIP44Path] = Try {
    val bip32 = BIP32Path(children)
    BIP44Path.fromString(bip32.toString)
  }

  def apply(
      coin: BIP44Coin,
      accountIndex: Int,
      chainType: BIP44ChainType,
      addressIndex: Int): BIP44Path = {

    val account = BIP44Account(coin = coin, index = accountIndex)
    val chain =
      BIP44Chain(account = account, chainType = chainType)
    val address = {
      BIP44Address(chain, addressIndex)
    }
    BIP44PathImpl(address)
  }

  def fromString(string: String): BIP44Path = {

    val bip32Path = BIP32Path.fromString(string)
    val children = bip32Path.path
    require(children.head == BIP32Node(PURPOSE, hardened = true),
            "The first child in a BIP44 path string must be 44'")
    require(children.length == 5, "A BIP44 path string must have five elements")

    val _ :+ coinChild :+ accountChild :+ chainChild :+ addressChild =
      children

    require(coinChild.hardened, "The coin type child must be hardened!")
    require(accountChild.hardened, "The account child must be hardened!")
    require(!chainChild.hardened, "The chain child must not be hardened!")
    require(!addressChild.hardened,
            "The address index child must not be hardened!")

    val chainType = BIP44ChainType.fromInt(chainChild.index)
    val coinType = BIP44Coin.fromInt(coinChild.index)

    apply(coinType,
          accountIndex = accountChild.index,
          chainType,
          addressIndex = addressChild.index)
  }
}
