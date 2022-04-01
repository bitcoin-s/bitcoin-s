package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCContactDb
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}

case class DLCContactDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCContactDb, InetSocketAddress]
    with SlickUtil[DLCContactDb, InetSocketAddress] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCContactTable] =
    TableQuery[DLCContactTable]

  override def createAll(
      ts: Vector[DLCContactDb]): Future[Vector[DLCContactDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(ids: Vector[
    InetSocketAddress]): Query[DLCContactTable, DLCContactDb, Seq] =
    table.filter(_.address.inSet(ids)).sortBy(_.alias)

  override def findByPrimaryKey(
      id: InetSocketAddress): Query[DLCContactTable, DLCContactDb, Seq] = {
    table
      .filter(_.address === id)
  }

  override def findAll(contacts: Vector[DLCContactDb]): Query[
    DLCContactTable,
    DLCContactDb,
    Seq] =
    findByPrimaryKeys(contacts.map(_.address))

  def delete(address: InetSocketAddress): Future[Int] = {
    val query = table.filter(_.address === address)
    database.run(query.delete)
  }

  def findByAlias(alias: String): Future[Vector[DLCContactDb]] = {
    if (alias.trim().isEmpty) findAll()
    else {
      val query =
        table.filter(_.alias.like(s"%${alias.trim()}%")).sortBy(_.alias)
      database.run(query.result).map(_.toVector)
    }
  }

  class DLCContactTable(tag: Tag)
      extends Table[DLCContactDb](tag, schemaName, "contacts") {

    def alias: Rep[String] = column("alias")

    def address: Rep[InetSocketAddress] = column("address", O.PrimaryKey)

    def memo: Rep[String] = column("memo")

    def * : ProvenShape[DLCContactDb] =
      (alias, address, memo) <> (DLCContactDb.tupled, DLCContactDb.unapply)
  }
}
