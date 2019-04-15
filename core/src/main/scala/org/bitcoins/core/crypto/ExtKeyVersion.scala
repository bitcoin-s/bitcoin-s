package org.bitcoins.core.crypto

import scodec.bits._
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.core.protocol.blockchain.MainNetChainParams
import org.bitcoins.core.protocol.blockchain.TestNetChainParams
import org.bitcoins.core.protocol.blockchain.RegTestNetChainParams
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.config.RegTest

sealed abstract class ExtKeyVersion extends NetworkElement

sealed abstract class ExtKeyPrivVersion extends ExtKeyVersion

sealed abstract class ExtKeyPubVersion extends ExtKeyVersion

/**
  * @see [[https://github.com/satoshilabs/slips/blob/master/slip-0132.md#registered-hd-version-bytes SLIP132]]
  * for a list of registered HD version bytes
  */
object ExtKeyVersion {
  private[bitcoins] val allPrivs: Vector[ExtKeyPrivVersion] = Vector(
    LegacyMainNetPriv,
    LegacyTestNet3Priv,
    SegWitMainNetPriv,
    SegWitTestNet3Priv,
    NestedSegWitMainNetPriv,
    NestedSegWitTestNet3Priv
  )
  private[bitcoins] val allPubs: Seq[ExtKeyPubVersion] = Vector(
    LegacyMainNetPub,
    LegacyTestNet3Pub,
    SegWitMainNetPub,
    SegWitTestNet3Pub,
    NestedSegWitMainNetPub,
    NestedSegWitTestNet3Pub
  )

  private[bitcoins] val all: Vector[ExtKeyVersion] = allPrivs ++ allPubs

  def apply(bytes: ByteVector): Option[ExtKeyVersion] =
    all.find(_.bytes == bytes)

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `xpub`
    */
  final case object LegacyMainNetPub extends ExtKeyPubVersion {
    override val bytes = hex"0x0488b21E"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `xprv`
    */
  final case object LegacyMainNetPriv extends ExtKeyPrivVersion {
    override val bytes = hex"0x0488ADE4"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `tpub`
    */
  final case object LegacyTestNet3Pub extends ExtKeyPubVersion {
    override val bytes = hex"0x043587CF"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `tprv`
    */
  final case object LegacyTestNet3Priv extends ExtKeyPrivVersion {
    override val bytes = hex"0x04358394"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `zpub`
    */
  final case object SegWitMainNetPub extends ExtKeyPubVersion {
    override val bytes = hex"0x04b24746"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `zprv`
    */
  final case object SegWitMainNetPriv extends ExtKeyPrivVersion {
    override val bytes = hex"0x04b2430c"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `vpub`
    */
  final case object SegWitTestNet3Pub extends ExtKeyPubVersion {
    override val bytes = hex"0x045f1cf6"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `vprv`
    */
  final case object SegWitTestNet3Priv extends ExtKeyPrivVersion {
    override val bytes = hex"0x045f18bc"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `ypub`
    *
    * @see [[https://github.com/Samourai-Wallet/ExtLibJ/blob/87fcb87f87ed86c38d4b82aefac6c59ec981bdad/java/com/samourai/wallet/util/FormatsUtilGeneric.java#L32 Samourai Wallet]]
    */
  final case object NestedSegWitMainNetPub extends ExtKeyPubVersion {
    override val bytes = hex"0x049D7CB2"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `yprv`
    *
    * @see [[https://github.com/Samourai-Wallet/ExtLibJ/blob/87fcb87f87ed86c38d4b82aefac6c59ec981bdad/java/com/samourai/wallet/util/FormatsUtilGeneric.java#L39 Samourai Wallet]]
    */
  final case object NestedSegWitMainNetPriv extends ExtKeyPrivVersion {
    override val bytes = hex"0x049D7878"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPublicKey ExtPublicKey]]
    * with this version makes keys start with `upub`
    *
    * @see [[https://github.com/trezor/trezor-firmware/blob/master/common/defs/bitcoin/bitcoin_testnet.json#L17 Trezor]]
    * @see [[https://github.com/Samourai-Wallet/ExtLibJ/blob/87fcb87f87ed86c38d4b82aefac6c59ec981bdad/java/com/samourai/wallet/util/FormatsUtilGeneric.java#L33 Samourai wallet]]
    */
  final case object NestedSegWitTestNet3Pub extends ExtKeyPubVersion {
    // Value stolen from Trezor lib, see link above
    // ByteVector.fromLong(71979618).toHex = 00000000044a5262
    override val bytes = hex"0x044a5262"
  }

  /**
    * Generating a [[org.bitcoins.core.crypto.ExtPrivateKey ExtPrivateKey]]
    * with this version makes keys start with `uprv`
    *
    * @see [[https://github.com/Samourai-Wallet/ExtLibJ/blob/87fcb87f87ed86c38d4b82aefac6c59ec981bdad/java/com/samourai/wallet/util/FormatsUtilGeneric.java#L40 Samourai wallet]]
    */
  final case object NestedSegWitTestNet3Priv extends ExtKeyPrivVersion {
    override val bytes = hex"0x044a4e28"
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
  // TODO fix this
  def fromChainParams(chainParams: ChainParams): ExtKeyPrivVersion =
    chainParams match {
      case MainNetChainParams => ExtKeyVersion.LegacyMainNetPriv
      case TestNetChainParams | RegTestNetChainParams =>
        ExtKeyVersion.LegacyTestNet3Priv
    }

  // TODO fix this
  def fromNetworkParameters(
      networkParameters: NetworkParameters): ExtKeyPrivVersion =
    networkParameters match {
      case MainNet => ExtKeyVersion.LegacyMainNetPriv
      case TestNet3 | RegTest =>
        ExtKeyVersion.LegacyTestNet3Priv
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
      case MainNetChainParams => ExtKeyVersion.LegacyMainNetPub
      case TestNetChainParams | RegTestNetChainParams =>
        ExtKeyVersion.LegacyTestNet3Pub

    }
}
