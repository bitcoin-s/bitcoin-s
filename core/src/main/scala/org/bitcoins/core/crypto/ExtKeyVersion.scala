package org.bitcoins.core.crypto

import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  MainNetChainParams,
  RegTestNetChainParams,
  TestNetChainParams
}
import scodec.bits.ByteVector

sealed abstract class ExtKeyVersion extends NetworkElement

sealed abstract class ExtKeyPrivVersion extends ExtKeyVersion

sealed abstract class ExtKeyPubVersion extends ExtKeyVersion

object ExtKeyVersion {
  private val all: Seq[ExtKeyVersion] =
    Seq(MainNetPriv, MainNetPub, TestNet3Pub, TestNet3Priv)

  def apply(bytes: ByteVector): Option[ExtKeyVersion] =
    all.find(_.bytes == bytes)

  final case object MainNetPub extends ExtKeyPubVersion {
    override val bytes = ByteVector(0x04, 0x88, 0xb2, 0x1E)
  }

  final case object MainNetPriv extends ExtKeyPrivVersion {
    override val bytes = ByteVector(0x04, 0x88, 0xAD, 0xE4)
  }

  final case object TestNet3Pub extends ExtKeyPubVersion {
    override val bytes = ByteVector(0x04, 0x35, 0x87, 0xCF)
  }

  final case object TestNet3Priv extends ExtKeyPrivVersion {
    override val bytes = ByteVector(0x04, 0x35, 0x83, 0x94)
  }
}

object ExtKeyPrivVersion {

  /**
    * Converts the given chain params into a ExtKeyPrivVersion.
    * Treats regtest and testnet the same.
    *
    * @see [[https://github.com/bcoin-org/bcoin/blob/7c64fd845cbae23751558efbe8e078e2ccbfbd30/lib/protocol/networks.js#L838 bcoin]]
    *     and [[https://github.com/bitcoinj/bitcoinj/blob/bfe2a195b62bcbf1d2e678969e541ebc3656ae17/core/src/main/java/org/bitcoinj/params/RegTestParams.java#L48 BitcoinJ]]
    */
  def fromChainParams(chainParams: ChainParams): ExtKeyPrivVersion =
    chainParams match {
      case MainNetChainParams => ExtKeyVersion.MainNetPriv
      case TestNetChainParams | RegTestNetChainParams =>
        ExtKeyVersion.TestNet3Priv
    }

  def fromNetworkParameters(
      networkParameters: NetworkParameters): ExtKeyPrivVersion =
    networkParameters match {
      case MainNet => ExtKeyVersion.MainNetPriv
      case TestNet3 | RegTest =>
        ExtKeyVersion.TestNet3Priv
    }

}

object ExtKeyPubVersion {

  /**
    * Converts the given chain params into a ExtKeyPubVersion.
    * Treats regtest and testnet the same.
    *
    * @see [[https://github.com/bcoin-org/bcoin/blob/7c64fd845cbae23751558efbe8e078e2ccbfbd30/lib/protocol/networks.js#L838 bcoin]]
    *     and [[https://github.com/bitcoinj/bitcoinj/blob/bfe2a195b62bcbf1d2e678969e541ebc3656ae17/core/src/main/java/org/bitcoinj/params/RegTestParams.java#L48 BitcoinJ]]
    */
  def fromChainParams(chainParams: ChainParams): ExtKeyPubVersion =
    chainParams match {
      case MainNetChainParams => ExtKeyVersion.MainNetPub
      case TestNetChainParams | RegTestNetChainParams =>
        ExtKeyVersion.TestNet3Pub

    }
}
