package org.bitcoins.wallet.models

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.bip44.{BIP44ChainType, BIP44Coin, BIP44Path}
import org.bitcoins.core.crypto.{
  ECPublicKey,
  ExtPrivateKey,
  Sha256Hash160Digest
}
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.util.CryptoUtil
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.ProvenShape

case class AddressDb(
    path: BIP44Path,
    ecPublicKey: ECPublicKey,
    hashedPubKey: Sha256Hash160Digest,
    address: BitcoinAddress,
    scriptType: ScriptType)

object AddressDbHelper {

  def getP2WPKHAddress(
      xpriv: ExtPrivateKey,
      path: BIP44Path,
      np: NetworkParameters): AddressDb = {

    val xprivAtPath: ExtPrivateKey = xpriv.deriveChildPrivKey(path)
    val pub = xprivAtPath.key.publicKey
    val witnessSpk = P2WPKHWitnessSPKV0(pub)
    val addr = Bech32Address(witnessSpk, np)
    AddressDb(path,
              pub,
              CryptoUtil.sha256Hash160(pub.bytes),
              addr,
              ScriptType.WITNESS_V0_KEYHASH)
  }
}

class AddressTable(tag: Tag) extends Table[AddressDb](tag, "addresses") {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def accountIndex: Rep[Int] = column[Int]("account_index")

  def accountCoin: Rep[BIP44Coin] = column[BIP44Coin]("bip44_coin")

  def accountChainType: Rep[BIP44ChainType] =
    column[BIP44ChainType]("bip44_chain_type")

  def addressIndex: Rep[Int] = column[Int]("address_index")

  def address: Rep[BitcoinAddress] =
    column[BitcoinAddress]("address", O.PrimaryKey)

  def ecPublicKey: Rep[ECPublicKey] = column[ECPublicKey]("pubkey")

  def hashedPubKey: Rep[Sha256Hash160Digest] =
    column[Sha256Hash160Digest]("hashed_pubkey")

  def scriptType: Rep[ScriptType] = column[ScriptType]("script_type")

  private type AddressTuple = (
      Int,
      BIP44Coin,
      BIP44ChainType,
      BitcoinAddress,
      Int,
      ECPublicKey,
      Sha256Hash160Digest,
      ScriptType)

  private val fromTuple: AddressTuple => AddressDb = {
    case (accountIndex,
          accountCoin,
          accountChain,
          address,
          addressIndex,
          pubKey,
          hashedPubKey,
          scriptType) =>
      AddressDb(
        path = BIP44Path(coin = accountCoin,
                         accountIndex = accountIndex,
                         chainType = accountChain,
                         addressIndex = addressIndex),
        ecPublicKey = pubKey,
        hashedPubKey = hashedPubKey,
        address = address,
        scriptType = scriptType
      )
  }

  private val toTuple: AddressDb => Option[AddressTuple] = {
    case AddressDb(path, pubKey, hashedPubKey, address, scriptType) =>
      Some(
        (path.account.index,
         path.coin,
         path.chain.chainType,
         address,
         path.address.index,
         pubKey,
         hashedPubKey,
         scriptType))
  }

  override def * : ProvenShape[AddressDb] =
    (accountIndex,
     accountCoin,
     accountChainType,
     address,
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
