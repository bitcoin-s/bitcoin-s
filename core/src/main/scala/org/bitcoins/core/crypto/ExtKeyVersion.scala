package org.bitcoins.core.crypto

import org.bitcoins.crypto.NetworkElement
import scodec.bits._

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
