package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{ExtPrivateKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.crypto.ECPublicKey

case class DLCPublicKeys(
    fundingKey: ECPublicKey,
    toLocalCETKey: ECPublicKey,
    finalAddress: BitcoinAddress) {
  require(
    fundingKey != toLocalCETKey,
    s"Cannot use same key for fundingKey and toLocalCETKey, got $fundingKey")
}

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

    val finalPubKey: ECPublicKey =
      extPubKey
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 2}"))
        .get
        .key

    DLCPublicKeys(
      fundingKey = fundingPubKey,
      toLocalCETKey = cetToLocalPubKey,
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
