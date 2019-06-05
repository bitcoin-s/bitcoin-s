package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.db.{CRUD, SlickUtil}
import slick.dbio.Effect
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.TableQuery
import slick.sql.SqlAction

import scala.concurrent.{ExecutionContext, Future}
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.wallet.config.WalletAppConfig

case class AddressDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig
) extends CRUD[AddressDb, BitcoinAddress] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table: TableQuery[AddressTable] = TableQuery[AddressTable]

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

  def findAll(): Future[Vector[AddressDb]] = {
    val query = table.result
    database.run(query).map(_.toVector)
  }

  private def addressesForAccountQuery(
      accountIndex: Int): Query[AddressTable, AddressDb, Seq] =
    table.filter(_.accountIndex === accountIndex)

  def findMostRecentChange(accountIndex: Int): Future[Option[AddressDb]] = {
    val query = findMostRecentForChain(accountIndex, HDChainType.Change)

    database.run(query)
  }

  private def findMostRecentForChain(
      accountIndex: Int,
      chain: HDChainType): SqlAction[Option[AddressDb], NoStream, Effect.Read] = {
    addressesForAccountQuery(accountIndex)
      .filter(_.accountChainType === chain)
      .sortBy(_.addressIndex.desc)
      .take(1)
      .result
      .headOption
  }

  def findMostRecentExternal(accountIndex: Int): Future[Option[AddressDb]] = {
    val query = findMostRecentForChain(accountIndex, HDChainType.External)
    database.run(query)
  }
}
