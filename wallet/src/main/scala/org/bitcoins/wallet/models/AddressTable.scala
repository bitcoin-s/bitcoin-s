package org.bitcoins.wallet.models

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.hd._
import org.bitcoins.core.crypto.{
  ECPublicKey,
  ExtPrivateKey,
  Sha256Hash160Digest
}
import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  ScriptWitness
}
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.script.ScriptType
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

// todo: make ADT for different addresses in DB, seeing as they have different fields
// todo: indicate whether or not address has been spent to
case class AddressDb(
    path: SegWitHDPath,
    ecPublicKey: ECPublicKey,
    hashedPubKey: Sha256Hash160Digest,
    address: BitcoinAddress,
    witnessScriptOpt: Option[ScriptWitness],
    scriptType: ScriptType)

object AddressDbHelper {

  /** Get a Segwit pay-to-pubkeyhash address */
  def getP2WPKHAddress(
      xpriv: ExtPrivateKey,
      path: SegWitHDPath,
      np: NetworkParameters): AddressDb = {

    val xprivAtPath: ExtPrivateKey = xpriv.deriveChildPrivKey(path)
    val pub = xprivAtPath.key.publicKey
    val witnessSpk = P2WPKHWitnessSPKV0(pub)
    val scriptWitness = P2WPKHWitnessV0(pub)
    val addr = Bech32Address(witnessSpk, np)
    AddressDb(
      path = path,
      ecPublicKey = pub,
      hashedPubKey = witnessSpk.pubKeyHash,
      address = addr,
      witnessScriptOpt = Some(scriptWitness),
      scriptType = ScriptType.WITNESS_V0_KEYHASH
    )
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
    case (purpose, // since segwit paths are hard coded ignore
          accountIndex,
          accountCoin,
          accountChain,
          address,
          scriptWitnessOpt,
          addressIndex,
          pubKey,
          hashedPubKey,
          scriptType) =>
      HDCoinType.Bitcoin

      AddressDb(
        SegWitHDPath(coinType = accountCoin,
                     accountIndex = accountIndex,
                     chainType = accountChain,
                     addressIndex = addressIndex),
        ecPublicKey = pubKey,
        hashedPubKey = hashedPubKey,
        address = address,
        witnessScriptOpt = scriptWitnessOpt,
        scriptType = scriptType
      )
  }

  private val toTuple: AddressDb => Option[AddressTuple] = {
    case AddressDb(path,
                   pubKey,
                   hashedPubKey,
                   address,
                   scriptWitnessOpt,
                   scriptType) =>
      Some(
        (path.purpose,
         path.account.index,
         path.coin.coinType,
         path.chain.chainType,
         address,
         scriptWitnessOpt,
         path.address.index,
         pubKey,
         hashedPubKey,
         scriptType))
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
      accountTable => (accountTable.coin, accountTable.index))
}
