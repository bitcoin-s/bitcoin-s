package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}

case class DLCPublicKeys(fundingKey: ECPublicKey, payoutAddress: BitcoinAddress)

object DLCPublicKeys {

  def fromPrivKeys(
      fundingPrivKey: ECPrivateKey,
      payoutKey: ECPrivateKey,
      network: BitcoinNetwork): DLCPublicKeys = {
    fromPubKeys(fundingPrivKey.publicKey, payoutKey.publicKey, network)
  }

  def fromPubKeys(
      fundingKey: ECPublicKey,
      payoutKey: ECPublicKey,
      network: BitcoinNetwork): DLCPublicKeys = {
    DLCPublicKeys(fundingKey,
                  Bech32Address(P2WPKHWitnessSPKV0(payoutKey), network))
  }
}
