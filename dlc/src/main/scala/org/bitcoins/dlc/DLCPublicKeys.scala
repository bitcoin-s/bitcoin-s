package org.bitcoins.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{ECPublicKey, ExtPrivateKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0

case class DLCPublicKeys(
    fundingKey: ECPublicKey,
    toLocalCETKey: ECPublicKey,
    toRemoteCETKey: ECPublicKey,
    finalAddress: BitcoinAddress)

object DLCPublicKeys {

  def fromExtPubKeyAndIndex(
      extPubKey: ExtPublicKey,
      nextAddressIndex: Int,
      network: BitcoinNetwork): DLCPublicKeys = {
    val fundingPubKey: ECPublicKey =
      extPubKey
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/$nextAddressIndex"))
        .get
        .key

    val cetToLocalPubKey: ECPublicKey =
      extPubKey
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 1}"))
        .get
        .key

    val cetToRemotePubKey: ECPublicKey =
      extPubKey
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 2}"))
        .get
        .key

    val finalPubKey: ECPublicKey =
      extPubKey
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 3}"))
        .get
        .key

    DLCPublicKeys(
      fundingKey = fundingPubKey,
      toLocalCETKey = cetToLocalPubKey,
      toRemoteCETKey = cetToRemotePubKey,
      finalAddress = Bech32Address(P2WPKHWitnessSPKV0(finalPubKey), network)
    )
  }

  def fromExtPrivKeyAndIndex(
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      network: BitcoinNetwork): DLCPublicKeys = {
    fromExtPubKeyAndIndex(extPrivKey.extPublicKey, nextAddressIndex, network)
  }
}
