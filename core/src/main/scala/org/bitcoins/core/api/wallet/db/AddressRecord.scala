package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.hd.*
import org.bitcoins.core.protocol.script.{
  ScriptPubKey,
  ScriptWitness,
  TaprootScriptPubKey,
  UnassignedWitnessScriptPubKey,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.{
  Bech32Address,
  Bech32mAddress,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
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
      case (HDPurpose.SegWit, bechAddr: Bech32Address, Some(scriptWitness)) =>
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

      case (HDPurpose.Legacy, legacyAddr: P2PKHAddress, None) =>
        val path = LegacyHDPath(coinType = accountCoin,
                                accountIndex = accountIndex,
                                chainType = accountChain,
                                addressIndex = addressIndex)
        LegacyAddressDb(path,
                        pubKey,
                        hashedPubKey,
                        legacyAddr,
                        scriptPubKey = scriptPubKey)

      case (HDPurpose.NestedSegWit,
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
      case (HDPurpose.Taproot, address: Bech32mAddress, None) =>
        address.scriptPubKey match {
          case x @ (_: WitnessScriptPubKeyV0 |
              _: UnassignedWitnessScriptPubKey) =>
            sys.error(
              s"Cannot convert non-taproot spk to TaprootAddressDb, got=$x")
          case t: TaprootScriptPubKey =>
            val path = TaprootHDPath(coin = accountCoin,
                                     accountIndex = accountIndex,
                                     chainType = accountChain,
                                     addressIndex = addressIndex)

            TaprootAddressDb(path = path,
                             ecPublicKey = pubKey,
                             address = address,
                             scriptPubKey = t)
        }

      case (purpose: HDPurpose, address: BitcoinAddress, scriptWitnessOpt) =>
        throw new IllegalArgumentException(
          s"Got invalid combination of HD purpose, address and script witness: $purpose, $address, $scriptWitnessOpt")
    }
  }
}

object AddressRecord {

  def fromAddressDb(addressDb: AddressDb, scriptPubKeyId: Long): AddressRecord =
    addressDb match {
      case t @ TaprootAddressDb(path, pubKey, address, scriptPubKey) =>
        AddressRecord(
          purpose = path.purpose,
          accountCoin = path.coin.coinType,
          accountIndex = path.account.index,
          accountChain = path.chain.chainType,
          addressIndex = path.address.index,
          address = address,
          pubKey = pubKey,
          hashedPubKey = t.hashedPubKey,
          scriptPubKeyId = scriptPubKeyId,
          scriptWitnessOpt = None
        )
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
