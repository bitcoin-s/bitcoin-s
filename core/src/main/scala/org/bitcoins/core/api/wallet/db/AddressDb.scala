package org.bitcoins.core.api.wallet.db

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.{
  Bech32Address,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
import org.bitcoins.core.script.ScriptType
import org.bitcoins.crypto.{ECPublicKey, Sha256Hash160Digest}

sealed trait AddressDb {
  protected type PathType <: HDPath

  def path: PathType
  def ecPublicKey: ECPublicKey
  def hashedPubKey: Sha256Hash160Digest
  def address: BitcoinAddress
  def scriptType: ScriptType
  def witnessScriptOpt: Option[ScriptWitness]
  def scriptPubKey: ScriptPubKey
}

/** Segwit P2PKH */
case class SegWitAddressDb(
    path: SegWitHDPath,
    ecPublicKey: ECPublicKey,
    hashedPubKey: Sha256Hash160Digest,
    address: Bech32Address,
    witnessScript: ScriptWitness,
    scriptPubKey: ScriptPubKey
) extends AddressDb {
  override type PathType = SegWitHDPath

  override val scriptType = ScriptType.WITNESS_V0_KEYHASH
  override val witnessScriptOpt = Some(witnessScript)
}

/** Segwit P2PKH-in-P2SH */
case class NestedSegWitAddressDb(
    path: NestedSegWitHDPath,
    ecPublicKey: ECPublicKey,
    hashedPubKey: Sha256Hash160Digest,
    address: P2SHAddress,
    witnessScript: ScriptWitness,
    scriptPubKey: ScriptPubKey
) extends AddressDb {
  override type PathType = NestedSegWitHDPath

  override val scriptType = ScriptType.SCRIPTHASH
  override val witnessScriptOpt = Some(witnessScript)
}

/** P2PKH */
case class LegacyAddressDb(
    path: LegacyHDPath,
    ecPublicKey: ECPublicKey,
    hashedPubKey: Sha256Hash160Digest,
    address: P2PKHAddress,
    scriptPubKey: ScriptPubKey
) extends AddressDb {
  override type PathType = LegacyHDPath

  override val scriptType = ScriptType.PUBKEYHASH
  override val witnessScriptOpt = None
}
// todo: make ADT for different addresses in DB, seeing as they have different fields
// todo: indicate whether or not address has been spent to

object AddressDbHelper {

  /** Get a Segwit pay-to-pubkeyhash address */
  def getSegwitAddress(
      pub: ECPublicKey,
      path: SegWitHDPath,
      np: NetworkParameters): SegWitAddressDb = {

    val witnessSpk = P2WPKHWitnessSPKV0(pub)
    val scriptWitness = P2WPKHWitnessV0(pub)
    val addr = Bech32Address(witnessSpk, np)
    SegWitAddressDb(
      path = path,
      ecPublicKey = pub,
      hashedPubKey = witnessSpk.pubKeyHash,
      address = addr,
      witnessScript = scriptWitness,
      scriptPubKey = witnessSpk
    )
  }

  /** Get a legacy pay-to-pubkeyhash address */
  def getLegacyAddress(
      pub: ECPublicKey,
      path: LegacyHDPath,
      np: NetworkParameters): LegacyAddressDb = {
    val spk = P2PKHScriptPubKey(pub)
    val addr = P2PKHAddress(spk, np)
    LegacyAddressDb(path = path,
                    ecPublicKey = pub,
                    hashedPubKey = spk.pubKeyHash,
                    address = addr,
                    scriptPubKey = spk)
  }

  /** Get a nested Segwit pay-to-pubkeyhash address */
  def getNestedSegwitAddress(
      pub: ECPublicKey,
      path: NestedSegWitHDPath,
      np: NetworkParameters): NestedSegWitAddressDb = {
    val redeem = P2WPKHWitnessSPKV0(pub)
    val spk = P2SHScriptPubKey(redeem)
    val scriptWitness = P2WPKHWitnessV0(pub)
    val addr = P2SHAddress(spk, np)
    NestedSegWitAddressDb(path = path,
                          ecPublicKey = pub,
                          hashedPubKey = redeem.pubKeyHash,
                          address = addr,
                          witnessScript = scriptWitness,
                          scriptPubKey = spk)
  }

  /** Gets an address. Derives the correct type by looking at the kind of path passed in */
  def getAddress(
      pub: ECPublicKey,
      path: HDPath,
      np: NetworkParameters): AddressDb =
    path match {
      case legacy: LegacyHDPath       => getLegacyAddress(pub, legacy, np)
      case nested: NestedSegWitHDPath => getNestedSegwitAddress(pub, nested, np)
      case segwit: SegWitHDPath       => getSegwitAddress(pub, segwit, np)
    }
}
