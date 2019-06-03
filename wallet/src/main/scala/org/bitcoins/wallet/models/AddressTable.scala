package org.bitcoins.wallet.models

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.hd._
import org.bitcoins.core.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  ScriptWitness
}
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.script.ScriptType
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape
import org.bitcoins.core.protocol.P2SHAddress
import org.bitcoins.core.protocol.P2PKHAddress
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey

sealed trait AddressDb {
  protected type PathType <: HDPath

  def path: PathType
  def ecPublicKey: ECPublicKey
  def hashedPubKey: Sha256Hash160Digest
  def address: BitcoinAddress
  def scriptType: ScriptType
  def witnessScriptOpt: Option[ScriptWitness]
}

/** Segwit P2PKH */
case class SegWitAddressDb(
    path: SegWitHDPath,
    ecPublicKey: ECPublicKey,
    hashedPubKey: Sha256Hash160Digest,
    address: Bech32Address,
    witnessScript: ScriptWitness
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
    address: P2SHAddress
) extends AddressDb {
  override type PathType = NestedSegWitHDPath

  override val scriptType = ScriptType.SCRIPTHASH
  override val witnessScriptOpt = None
}

/** P2PKH */
case class LegacyAddressDb(
    path: LegacyHDPath,
    ecPublicKey: ECPublicKey,
    hashedPubKey: Sha256Hash160Digest,
    address: P2PKHAddress
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
      witnessScript = scriptWitness
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
                    address = addr)
  }

  /** Get a nested Segwit pay-to-pubkeyhash address */
  def getNestedSegwitAddress(
      pub: ECPublicKey,
      path: NestedSegWitHDPath,
      np: NetworkParameters): NestedSegWitAddressDb = {
    ???
  }
}

/**
  * todo: this needs design rework.
  * todo: https://github.com/bitcoin-s/bitcoin-s-core/pull/391#discussion_r274188334
  */
class AddressTable(tag: Tag) extends Table[AddressDb](tag, "addresses") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def purpose: Rep[HDPurpose] = column[HDPurpose]("hd_purpose")

  def accountIndex: Rep[Int] = column[Int]("account_index")

  def accountCoin: Rep[HDCoinType] = column[HDCoinType]("hd_coin")

  def accountChainType: Rep[HDChainType] =
    column[HDChainType]("hd_chain_type")

  def addressIndex: Rep[Int] = column[Int]("address_index")

  def address: Rep[BitcoinAddress] =
    column[BitcoinAddress]("address", O.PrimaryKey)

  def ecPublicKey: Rep[ECPublicKey] = column[ECPublicKey]("pubkey")

  def hashedPubKey: Rep[Sha256Hash160Digest] =
    column[Sha256Hash160Digest]("hashed_pubkey")

  def scriptType: Rep[ScriptType] = column[ScriptType]("script_type")

  def scriptWitness: Rep[Option[ScriptWitness]] =
    column[Option[ScriptWitness]]("script_witness")

  private type AddressTuple = (
      HDPurpose,
      Int,
      HDCoinType,
      HDChainType,
      BitcoinAddress,
      Option[ScriptWitness],
      Int,
      ECPublicKey,
      Sha256Hash160Digest,
      ScriptType)

  private val fromTuple: AddressTuple => AddressDb = {
    case (
        purpose,
        accountIndex,
        accountCoin,
        accountChain,
        address,
        scriptWitnessOpt,
        addressIndex,
        pubKey,
        hashedPubKey,
        scriptType @ _ // what should we do about this? scriptType is inferrable from purpose
        ) =>
      (purpose, address, scriptWitnessOpt) match {
        case (HDPurposes.SegWit,
              bechAddr: Bech32Address,
              Some(scriptWitness)) =>
          val path =
            SegWitHDPath(coinType = accountCoin,
                         accountIndex = accountIndex,
                         chainType = accountChain,
                         addressIndex = addressIndex)

          SegWitAddressDb(path,
                          ecPublicKey = pubKey,
                          hashedPubKey = hashedPubKey,
                          address = bechAddr,
                          witnessScript = scriptWitness)

        case (HDPurposes.Legacy, legacyAddr: P2PKHAddress, None) =>
          val path = LegacyHDPath(coinType = accountCoin,
                                  accountIndex = accountIndex,
                                  chainType = accountChain,
                                  addressIndex = addressIndex)
          LegacyAddressDb(path, pubKey, hashedPubKey, legacyAddr)

        case (purpose: HDPurpose, address: BitcoinAddress, scriptWitnessOpt) =>
          throw new IllegalArgumentException(
            s"Got invalid combination of HD purpose, address and script witness: $purpose, $address, $scriptWitnessOpt" +
              s"Note: Currently only segwit addreses are implemented")
      }
  }

  private val toTuple: AddressDb => Option[AddressTuple] = {
    case SegWitAddressDb(path, pubKey, hashedPubKey, address, scriptWitness) =>
      Some(
        (path.purpose,
         path.account.index,
         path.coin.coinType,
         path.chain.chainType,
         address,
         Some(scriptWitness),
         path.address.index,
         pubKey,
         hashedPubKey,
         ScriptType.WITNESS_V0_KEYHASH))
    case LegacyAddressDb(path, pubkey, hashedPub, address) =>
      Some(
        path.purpose,
        path.account.index,
        path.coin.coinType,
        path.chain.chainType,
        address,
        None, // scriptwitness
        path.address.index,
        pubkey,
        hashedPub,
        ScriptType.PUBKEYHASH
      )
    case _: NestedSegWitAddressDb =>
      throw new RuntimeException(s"Nested segwit is not implemented yet!")

  }

  override def * : ProvenShape[AddressDb] =
    (purpose,
     accountIndex,
     accountCoin,
     accountChainType,
     address,
     scriptWitness,
     addressIndex,
     ecPublicKey,
     hashedPubKey,
     scriptType) <> (fromTuple, toTuple)

  val accounts = TableQuery[AccountTable]

  // for some reason adding a type annotation here causes compile error
  def fk =
    foreignKey("fk_account", (accountCoin, accountIndex), accounts)(
      accountTable => (accountTable.coinType, accountTable.index))
}
