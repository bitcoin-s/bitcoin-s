package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.bip44.BIP44ChainType
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.db.{CRUD, DbConfig, SlickUtil}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

case class AddressDAO(dbConfig: DbConfig, chainParams: ChainParams)(
    implicit executionContext: ExecutionContext)
    extends CRUD[AddressDb, BitcoinAddress] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val ec: ExecutionContext = executionContext

  override val table: TableQuery[AddressTable] = TableQuery[AddressTable]

  override def createAll(ts: Vector[AddressDb]): Future[Vector[AddressDb]] =
    SlickUtil.createAllNoAutoInc(ts, database, table)

  /** Finds the rows that correlate to the given primary keys */
  override def findByPrimaryKeys(
      addresses: Vector[BitcoinAddress]): Query[Table[_], AddressDb, Seq] =
    table.filter(_.address.inSet(addresses))

  override def findAll(ts: Vector[AddressDb]): Query[Table[_], AddressDb, Seq] =
    findByPrimaryKeys(ts.map(_.address))

  def findAll(): Future[Vector[AddressDb]] = {
    val query = table.result
    database.run(query).map(_.toVector)
  }

  def findMostRecentChange: Future[Option[AddressDb]] = findAll().map {
    addresses =>
      val change = addresses
        .filter(_.path.chain.chainType == BIP44ChainType.Change)

      change
        .sortBy(_.path.address.index)
        .lastOption
  }

  def findMostRecentExternal(accountIndex: Int): Future[Option[AddressDb]] =
    findAll().map { addreses =>
      val external =
        addreses.filter(_.path.chain.chainType == BIP44ChainType.External)

      external.sortBy(_.path.address.index).lastOption
    }
}
