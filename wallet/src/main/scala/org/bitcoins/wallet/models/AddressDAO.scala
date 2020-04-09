package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.core.hd.{HDAccount, HDChainType}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config.WalletAppConfig
import slick.dbio.Effect
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.TableQuery
import slick.sql.SqlAction

import scala.concurrent.{ExecutionContext, Future}

case class AddressDAO()(
    implicit ec: ExecutionContext,
    config: WalletAppConfig
) extends CRUD[AddressDb, BitcoinAddress] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[AddressTable] = TableQuery[AddressTable]
  private val spendingInfoTable = TableQuery[SpendingInfoTable]

  override def createAll(ts: Vector[AddressDb]): Future[Vector[AddressDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  /** Finds the rows that correlate to the given primary keys */
  override def findByPrimaryKeys(
      addresses: Vector[BitcoinAddress]): Query[Table[_], AddressDb, Seq] =
    table.filter(_.address.inSet(addresses))

  override def findAll(ts: Vector[AddressDb]): Query[Table[_], AddressDb, Seq] =
    findByPrimaryKeys(ts.map(_.address))

  def findAddress(addr: BitcoinAddress): Future[Option[AddressDb]] = {
    val query = findByPrimaryKey(addr).result
    database.run(query).map(_.headOption)
  }

  private def addressesForAccountQuery(
      accountIndex: Int): Query[AddressTable, AddressDb, Seq] =
    table.filter(_.accountIndex === accountIndex)

  /**
    * Finds the most recent change address in the wallet, if any
    */
  def findMostRecentChange(hdAccount: HDAccount): Future[Option[AddressDb]] = {
    val query =
      findMostRecentForChain(hdAccount, HDChainType.Change)

    database.run(query)
  }

  /** Finds all public keys in the wallet */
  def findAllPubkeys(): Future[Vector[ECPublicKey]] = {
    val query = table.map(_.ecPublicKey).distinct
    database.run(query.result).map(_.toVector)
  }

  /** Finds all SPKs in the wallet */
  def findAllSPKs(): Future[Vector[ScriptPubKey]] = {
    val query = table.map(_.scriptPubKey).distinct
    database.run(query.result).map(_.toVector)
  }

  def getUnusedAddresses: Future[Vector[AddressDb]] = {
    val query = {
      val joined =
        table.joinLeft(spendingInfoTable).on(_.scriptPubKey === _.scriptPubKey)
      joined.filter(_._2.isEmpty)
    }

    database.runVec(query.result).map(_.map(_._1))
  }

  def getUnusedAddresses(hdAccount: HDAccount): Future[Vector[AddressDb]] = {
    getUnusedAddresses.map(_.filter(_.path.account == hdAccount))
  }

  private def findMostRecentForChain(
      account: HDAccount,
      chain: HDChainType): SqlAction[Option[AddressDb], NoStream, Effect.Read] = {
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
    database.run(query)
  }
}
