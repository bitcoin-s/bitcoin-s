package org.bitcoins.wallet.models

import org.bitcoins.core.hd.{
  HDChainType,
  HDCoinType,
  HDPurpose,
  HDPurposes,
  LegacyHDPath,
  NestedSegWitHDPath,
  SegWitHDPath
}
import org.bitcoins.core.protocol.{
  Bech32Address,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.crypto.{ECPublicKey, Sha256Hash160Digest}

case class AddressRecord(
    purpose: HDPurpose,
    accountCoin: HDCoinType,
    accountIndex: Int,
    accountChain: HDChainType,
    addressIndex: Int,
    address: BitcoinAddress,
    pubKey: ECPublicKey,
    hashedPubKey: Sha256Hash160Digest,
    scriptPubKeyId: Long,
    scriptWitnessOpt: Option[ScriptWitness]
) {

  def toAddressDb(scriptPubKey: ScriptPubKey): AddressDb = {
    (purpose, address, scriptWitnessOpt) match {
      case (HDPurposes.SegWit, bechAddr: Bech32Address, Some(scriptWitness)) =>
        val path =
          SegWitHDPath(coinType = accountCoin,
                       accountIndex = accountIndex,
                       chainType = accountChain,
                       addressIndex = addressIndex)

        SegWitAddressDb(path,
                        ecPublicKey = pubKey,
                        hashedPubKey = hashedPubKey,
                        address = bechAddr,
                        witnessScript = scriptWitness,
                        scriptPubKey = scriptPubKey)

      case (HDPurposes.Legacy, legacyAddr: P2PKHAddress, None) =>
        val path = LegacyHDPath(coinType = accountCoin,
                                accountIndex = accountIndex,
                                chainType = accountChain,
                                addressIndex = addressIndex)
        LegacyAddressDb(path,
                        pubKey,
                        hashedPubKey,
                        legacyAddr,
                        scriptPubKey = scriptPubKey)

      case (HDPurposes.NestedSegWit,
            address: P2SHAddress,
            Some(scriptWitness)) =>
        val path = NestedSegWitHDPath(coinType = accountCoin,
                                      accountIndex = accountIndex,
                                      chainType = accountChain,
                                      addressIndex = addressIndex)
        NestedSegWitAddressDb(path,
                              pubKey,
                              hashedPubKey,
                              address,
                              witnessScript = scriptWitness,
                              scriptPubKey = scriptPubKey)
      case (purpose: HDPurpose, address: BitcoinAddress, scriptWitnessOpt) =>
        throw new IllegalArgumentException(
          s"Got invalid combination of HD purpose, address and script witness: $purpose, $address, $scriptWitnessOpt")
    }
  }
}

object AddressRecord {

  def fromAddressDb(addressDb: AddressDb, scriptPubKeyId: Long): AddressRecord =
    addressDb match {
      case SegWitAddressDb(path,
                           pubKey,
                           hashedPubKey,
                           address,
                           scriptWitness,
                           _) =>
        AddressRecord(
          path.purpose,
          path.coin.coinType,
          path.account.index,
          path.chain.chainType,
          path.address.index,
          address,
          pubKey,
          hashedPubKey,
          scriptPubKeyId,
          Some(scriptWitness)
        )
      case LegacyAddressDb(path, pubkey, hashedPub, address, _) =>
        AddressRecord(
          path.purpose,
          path.coin.coinType,
          path.account.index,
          path.chain.chainType,
          path.address.index,
          address,
          pubkey,
          hashedPub,
          scriptPubKeyId,
          None // scriptwitness
        )
      case NestedSegWitAddressDb(path,
                                 pubKey,
                                 hashedPubKey,
                                 address,
                                 scriptWitness,
                                 _) =>
        AddressRecord(
          path.purpose,
          path.coin.coinType,
          path.account.index,
          path.chain.chainType,
          path.address.index,
          address,
          pubKey,
          hashedPubKey,
          scriptPubKeyId,
          Some(scriptWitness)
        )
    }
}
