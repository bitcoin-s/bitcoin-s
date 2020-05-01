package org.bitcoins.core.hd

import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#Coin_type BIP44]],
  * [[https://github.com/bitcoin/bips/blob/master/bip-0084.mediawiki BIP84]]
  * and
  * [[https://github.com/bitcoin/bips/blob/master/bip-0049.mediawiki BIP49]]
  * coin type.
  */
sealed trait HDCoinType {
  def toInt: Int
}

/**
  * @see [[https://github.com/satoshilabs/slips/blob/master/slip-0044.md SLIP-0044]]
  *     central registry of coin types
  */
object HDCoinType {

  final case object Bitcoin extends HDCoinType {
    override val toInt: Int = 0
  }

  final case object Testnet extends HDCoinType {
    override val toInt: Int = 1
  }

  def fromInt(int: Int): HDCoinType =
    int match {
      case Bitcoin.toInt => Bitcoin
      case Testnet.toInt => Testnet
      case _: Int =>
        throw new IllegalArgumentException(s"$int is not valid coin type!")
    }

  def fromNetwork(np: NetworkParameters): HDCoinType = {
    np match {
      case MainNet            => Bitcoin
      case TestNet3 | RegTest => Testnet
    }
  }

  def fromNode(node: BIP32Node): HDCoinType = {
    require(node.hardened,
            s"Cannot construct HDCoinType from un-hardened node: $node")

    fromInt(node.index)
  }
}
