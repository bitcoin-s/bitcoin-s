package org.bitcoins.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{
  HDAccount,
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
import org.bitcoins.core.script.ScriptType
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.{ECPublicKey, Sha256Hash160Digest}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config.WalletAppConfig
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class AddressDAO()(implicit
    ec: ExecutionContext,
    config: WalletAppConfig
) extends CRUD[AddressDb, BitcoinAddress]
    with SlickUtil[AddressDb, BitcoinAddress] {
  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  override val table: profile.api.TableQuery[AddressTable] =
    TableQuery[AddressTable]

  private lazy val spendingInfoTable: profile.api.TableQuery[
    SpendingInfoDAO#SpendingInfoTable] = {
    SpendingInfoDAO().table
  }

  private lazy val accountTable: slick.lifted.TableQuery[
    AccountDAO#AccountTable] = {
    AccountDAO().table
  }

  override def createAll(ts: Vector[AddressDb]): Future[Vector[AddressDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  /** Finds the rows that correlate to the given primary keys */
  override def findByPrimaryKeys(
      addresses: Vector[BitcoinAddress]): Query[AddressTable, AddressDb, Seq] =
    table.filter(_.address.inSet(addresses))

  override def findAll(
      ts: Vector[AddressDb]): Query[AddressTable, AddressDb, Seq] =
    findByPrimaryKeys(ts.map(_.address))

  def findAddress(addr: BitcoinAddress): Future[Option[AddressDb]] = {
    val query = findByPrimaryKey(addr).result
    database.run(query).map(_.headOption)
  }

  private def addressesForAccountQuery(
      accountIndex: Int): Query[AddressTable, AddressDb, Seq] =
    table.filter(_.accountIndex === accountIndex)

  def findAllForAccount(account: HDAccount): Future[Vector[AddressDb]] = {
    val query = table
      .filter(_.accountIndex === account.index)
      .filter(_.accountCoin === account.coin.coinType)

    database.run(query.result).map(_.toVector)
  }

  /**
    * Finds the most recent change address in the wallet, if any
    */
  def findMostRecentChange(hdAccount: HDAccount): Future[Option[AddressDb]] = {
    val query =
      findMostRecentForChain(hdAccount, HDChainType.Change)

    safeDatabase.run(query)
  }

  /** Finds all public keys in the wallet */
  def findAllPubkeys(): Future[Vector[ECPublicKey]] = {
    val query = table.map(_.ecPublicKey).distinct
    safeDatabase.run(query.result).map(_.toVector)
  }

  /** Finds all SPKs in the wallet */
  def findAllSPKs(): Future[Vector[ScriptPubKey]] = {
    val query = table.map(_.scriptPubKey).distinct
    safeDatabase.run(query.result).map(_.toVector)
  }

  def getUnusedAddresses: Future[Vector[AddressDb]] = {
    val query: slick.lifted.Query[
      (AddressTable, _),
      (AddressTable#TableElementType, Option[SpendingInfoDb]),
      Seq] = {
      val joined =
        table
          .joinLeft(spendingInfoTable)
          .on(_.scriptPubKey === _.scriptPubKey)
      joined.filter(_._2.isEmpty)
    }

    safeDatabase.runVec(query.result).map(_.map(_._1))
  }

  def getUnusedAddresses(hdAccount: HDAccount): Future[Vector[AddressDb]] = {
    getUnusedAddresses.map(_.filter(_.path.account == hdAccount))
  }

  def getSpentAddresses: Future[Vector[AddressDb]] = {
    val query = table
      .join(spendingInfoTable)
      .on(_.scriptPubKey === _.scriptPubKey)
      .filter(_._2.state.inSet(TxoState.spentStates))
      .map(_._1)

    safeDatabase.runVec(query.result)
  }

  def getFundedAddresses: Future[Vector[(AddressDb, CurrencyUnit)]] = {
    val query = table
      .join(spendingInfoTable)
      .on(_.scriptPubKey === _.scriptPubKey)
      .filter(_._2.state.inSet(TxoState.receivedStates))

    safeDatabase
      .runVec(query.result)
      .map(_.map {
        case (addrDb, utxoDb) => (addrDb, utxoDb.output.value)
      })
  }

  private def findMostRecentForChain(
      account: HDAccount,
      chain: HDChainType): slick.sql.SqlAction[
    Option[AddressDb],
    NoStream,
    Effect.Read] = {
    addressesForAccountQuery(account.index)
      .filter(_.purpose === account.purpose)
      .filter(_.accountCoin === account.coin.coinType)
      .filter(_.accountChainType === chain)
      .sortBy(_.addressIndex.desc)
      .take(1)
      .result
      .headOption
  }

  /**
    * Finds the most recent external address in the wallet, if any
    */
  def findMostRecentExternal(
      hdAccount: HDAccount): Future[Option[AddressDb]] = {
    val query =
      findMostRecentForChain(hdAccount, HDChainType.External)
    safeDatabase.run(query)
  }

  /**
    * todo: this needs design rework.
    * todo: https://github.com/bitcoin-s/bitcoin-s-core/pull/391#discussion_r274188334
    */
  class AddressTable(tag: Tag) extends Table[AddressDb](tag, "addresses") {

    def purpose: Rep[HDPurpose] = column("hd_purpose")

    def accountCoin: Rep[HDCoinType] = column("hd_coin")

    def accountIndex: Rep[Int] = column("account_index")

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
      case (purpose,
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
          case (purpose: HDPurpose,
                address: BitcoinAddress,
                scriptWitnessOpt) =>
            throw new IllegalArgumentException(
              s"Got invalid combination of HD purpose, address and script witness: $purpose, $address, $scriptWitnessOpt")
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
      case NestedSegWitAddressDb(path,
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
           ScriptType.SCRIPTHASH))
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

    def fk: ForeignKeyQuery[_, AccountDb] =
      foreignKey("fk_account",
                 sourceColumns = (purpose, accountCoin, accountIndex),
                 targetTableQuery = accountTable) { accountTable =>
        (accountTable.purpose, accountTable.coinType, accountTable.index)
      }
  }
}
