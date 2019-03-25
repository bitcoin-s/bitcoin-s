package org.bitcoins.core.crypto.bip44
import org.bitcoins.core.crypto.bip32.{BIP32Node, BIP32Path}
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  MainNetChainParams,
  RegTestNetChainParams,
  TestNetChainParams
}

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]]
  * coin type.
  */
sealed abstract class BIP44Coin extends BIP32Path {
  override def path: Vector[BIP32Node] =
    Vector(BIP44Path.purposeChild, BIP32Node(toInt, hardened = true))

  def toInt: Int

  def toAccount(index: Int): BIP44Account = BIP44Account(this, index)
}

/**
  * @see [[https://github.com/satoshilabs/slips/blob/master/slip-0044.md SLIP-0044]]
  *     central registry of coin types
  */
object BIP44Coin {

  final case object Bitcoin extends BIP44Coin {
    override val toInt: Int = 0
  }

  /**
    * All coins
    */
  final case object Testnet extends BIP44Coin {
    override val toInt: Int = 1
  }

  /**
    * Converts the given chain params into a BIP44 coin type. Treats regtest and
    * testnet the same.
    *
    * @see [[https://github.com/bcoin-org/bcoin/blob/7c64fd845cbae23751558efbe8e078e2ccbfbd30/lib/protocol/networks.js#L838 bcoin]]
    *     and [[https://github.com/bitcoinj/bitcoinj/blob/bfe2a195b62bcbf1d2e678969e541ebc3656ae17/core/src/main/java/org/bitcoinj/params/RegTestParams.java#L48 BitcoinJ]]
    */
  def fromChainParams(chainParams: ChainParams): BIP44Coin = chainParams match {
    case MainNetChainParams                         => Bitcoin
    case TestNetChainParams | RegTestNetChainParams => Testnet
  }

  def fromInt(int: Int): BIP44Coin =
    int match {
      case Bitcoin.toInt => Bitcoin
      case Testnet.toInt => Testnet
      case _: Int =>
        throw new IllegalArgumentException(s"$int is not valid coin type!")
    }
}
