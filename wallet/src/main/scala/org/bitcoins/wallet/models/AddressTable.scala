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
import org.bitcoins.core.protocol.script.ScriptPubKey

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
    scriptPubKey: ScriptPubKey
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
    ???
  }

  /** Gets an address. Derives the correct type by looking at the kind of path passed in */
  def getAddress(
      pub: ECPublicKey,
      path: HDPath,
      np: NetworkParameters): AddressDb = path match {
    case legacy: LegacyHDPath       => getLegacyAddress(pub, legacy, np)
    case nested: NestedSegWitHDPath => getNestedSegwitAddress(pub, nested, np)
    case segwit: SegWitHDPath       => getSegwitAddress(pub, segwit, np)
  }
}

/**
  * todo: this needs design rework.
  * todo: https://github.com/bitcoin-s/bitcoin-s-core/pull/391#discussion_r274188334
  */
class AddressTable(tag: Tag) extends Table[AddressDb](tag, "addresses") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def purpose: Rep[HDPurpose] = column("hd_purpose")

  def accountIndex: Rep[Int] = column("account_index")

  def accountCoin: Rep[HDCoinType] = column("hd_coin")

  def accountChainType: Rep[HDChainType] = column("hd_chain_type")

  def addressIndex: Rep[Int] = column("address_index")

  def address: Rep[BitcoinAddress] = column("address", O.PrimaryKey)

  def ecPublicKey: Rep[ECPublicKey] = column("pubkey")

  def hashedPubKey: Rep[Sha256Hash160Digest] = column("hashed_pubkey")

  def scriptType: Rep[ScriptType] = column("script_type")

  def scriptPubKey: Rep[ScriptPubKey] = column("script_pub_key", O.Unique)

  def scriptWitness: Rep[Option[ScriptWitness]] = column("script_witness")

  private type AddressTuple = (
      HDPurpose,
      Int,
      HDCoinType,
      HDChainType,
      BitcoinAddress,
      Option[ScriptWitness],
      ScriptPubKey,
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
        scriptPubKey,
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

        case (purpose: HDPurpose, address: BitcoinAddress, scriptWitnessOpt) =>
          throw new IllegalArgumentException(
            s"Got invalid combination of HD purpose, address and script witness: $purpose, $address, $scriptWitnessOpt" +
              s"Note: Currently only segwit addreses are implemented")
      }
  }

  private val toTuple: AddressDb => Option[AddressTuple] = {
    case SegWitAddressDb(path,
                         pubKey,
                         hashedPubKey,
                         address,
                         scriptWitness,
                         scriptPubKey) =>
      Some(
        (path.purpose,
         path.account.index,
         path.coin.coinType,
         path.chain.chainType,
         address,
         Some(scriptWitness),
         scriptPubKey,
         path.address.index,
         pubKey,
         hashedPubKey,
         ScriptType.WITNESS_V0_KEYHASH))
    case LegacyAddressDb(path, pubkey, hashedPub, address, scriptPubKey) =>
      Some(
        path.purpose,
        path.account.index,
        path.coin.coinType,
        path.chain.chainType,
        address,
        None, // scriptwitness
        scriptPubKey,
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
     scriptPubKey,
     addressIndex,
     ecPublicKey,
     hashedPubKey,
     scriptType) <> (fromTuple, toTuple)

  val accounts = TableQuery[AccountTable]

  // for some reason adding a type annotation here causes compile error
  def fk =
    foreignKey("fk_account",
               sourceColumns = (purpose, accountCoin, accountIndex),
               targetTableQuery = accounts) { accountTable =>
      (accountTable.purpose, accountTable.coinType, accountTable.index)
    }
}
