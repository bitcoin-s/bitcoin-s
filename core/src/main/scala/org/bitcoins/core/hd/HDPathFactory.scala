package org.bitcoins.core.hd

import org.bitcoins.crypto.StringFactory

import scala.util.Try

private[hd] trait HDPathFactory[PathType <: BIP32Path]
    extends StringFactory[PathType] {

  private lazy val pathName = getClass.getSimpleName

  def apply(
      coin: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): PathType

  /** Tries to generate a HD path from the given path segments
    */
  def apply(children: Vector[BIP32Node]): Try[PathType] =
    Try {
      val bip32 = BIP32Path(children)
      fromString(bip32.toString)
    }

  /** Gets a HD path from the given HD address */
  def apply(address: HDAddress): PathType = {
    apply(
      coin = address.coin.coinType,
      accountIndex = address.account.index,
      chainType = address.chain.chainType,
      addressIndex = address.index
    )
  }

  /** Parses a string representation of a HD path */
  override def fromString(string: String): PathType = {

    val bip32Path = BIP32Path.fromString(string)
    val children = bip32Path.path
    val maybePurpose = children.head

    val purpose: HDPurpose = maybePurpose match {
      case BIP32Node(_, false) =>
        throw new IllegalArgumentException(
          "The first child in a HD path must be hardened")
      case BIP32Node(HDPurposes.Legacy.constant, true) => HDPurposes.Legacy
      case BIP32Node(HDPurposes.SegWit.constant, true) => HDPurposes.SegWit
      case BIP32Node(HDPurposes.NestedSegWit.constant, true) =>
        HDPurposes.NestedSegWit
      case BIP32Node(HDPurposes.Multisig.constant, true) => HDPurposes.Multisig
      case BIP32Node(unknown, true) =>
        throw new IllegalArgumentException(
          s"Purpose constant ($unknown) is not a known purpose constant")
    }

    require(
      purpose.constant == PURPOSE,
      s"Expected $PURPOSE' as the purpose constant, got ${purpose.constant}'")

    require(children.length == 5,
            s"A $pathName path string must have five elements")

    val _ :+ coinChild :+ accountChild :+ chainChild :+ addressChild =
      children

    require(coinChild.hardened, "The coin type child must be hardened!")
    require(accountChild.hardened, "The account child must be hardened!")
    require(!chainChild.hardened, "The chain child must not be hardened!")
    require(!addressChild.hardened,
            "The address index child must not be hardened!")

    val chainType = HDChainType.fromInt(chainChild.index)
    val coinType = HDCoinType(coinChild.index)

    apply(coin = coinType,
          accountIndex = accountChild.index,
          chainType = chainType,
          addressIndex = addressChild.index)
  }

  protected def assembleAddress(
      coinType: HDCoinType,
      accountIndex: Int,
      chainType: HDChainType,
      addressIndex: Int): HDAddress = {
    val coin = HDCoin(hdPurpose, coinType)
    val account = HDAccount(coin = coin, index = accountIndex)
    val chain =
      HDChain(account = account, chainType = chainType)
    HDAddress(chain, addressIndex)
  }

  /** The purpose constant from BIP43
    *
    * @see [[https://github.com/bitcoin/bips/blob/master/bip-0043.mediawiki#purpose BIP43]]
    */
  def PURPOSE: Int

  protected lazy val hdPurpose: HDPurpose =
    HDPurposes.fromConstant(PURPOSE).get // todo

  lazy val purposeChild: BIP32Node = BIP32Node(PURPOSE, hardened = true)

  /** The index of the coin segement of a BIP44 path
    */
  final val COIN_INDEX: Int = 1

  /** The index of the account segement of a BIP44 path
    */
  final val ACCOUNT_INDEX: Int = 2

  /** The index of the chain segement of a BIP44 path
    */
  final val CHAIN_INDEX: Int = 3

  /** The index of the address segement of a BIP44 path
    */
  final val ADDRESS_INDEX: Int = 4
}
