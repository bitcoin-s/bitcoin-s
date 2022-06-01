package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.{
  DLCContactDb,
  DLCContactMapping,
  DLCContactMappingDb,
  DLCDb
}
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.ForeignKeyQuery

import java.net.InetSocketAddress
import scala.concurrent.{ExecutionContext, Future}

case class DLCContactMappingDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCContactMappingDb, Sha256Digest]
    with SlickUtil[DLCContactMappingDb, Sha256Digest] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._

  import profile.api._

  /** The table inside our database we are inserting into */
  override val table: TableQuery[DLCContactMappingTable] =
    TableQuery[DLCContactMappingTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  private lazy val contactTable: slick.lifted.TableQuery[
    DLCContactDAO#DLCContactTable] = {
    DLCContactDAO().table
  }

  override def createAll(
      ts: Vector[DLCContactMappingDb]): Future[Vector[DLCContactMappingDb]] = {
    createAllNoAutoInc(ts, safeDatabase)
  }

  def create(
      dlcDb: DLCDb,
      contactDb: DLCContactDb): Future[DLCContactMappingDb] = {
    create(DLCContactMappingDb(dlcDb.dlcId, contactDb.address))
  }

  def create(
      dlcId: Sha256Digest,
      contactId: InetSocketAddress): Future[DLCContactMappingDb] = {
    create(DLCContactMappingDb(dlcId, contactId))
  }

  def createIfContactExists(
      dlcId: Sha256Digest,
      contactId: InetSocketAddress): Future[Option[DLCContactDb]] = {
    val action = for {
      contactOpt <- contactTable
        .filter(_.address === contactId)
        .result
        .headOption
      res <-
        if (contactOpt.nonEmpty) {
          val db = DLCContactMappingDb(dlcId, contactId)
          (table += db).map(_ => contactOpt)
        } else {
          DBIO.successful(None)
        }
    } yield res

    safeDatabase.run(action)
  }

  def upsert(
      dlcId: Sha256Digest,
      contactId: InetSocketAddress): Future[DLCContactMappingDb] = {
    upsert(DLCContactMappingDb(dlcId, contactId))
  }

  def delete(dlcId: Sha256Digest): Future[Unit] = {
    safeDatabase.run(table.filter(_.dlcId === dlcId).delete).map(_ => ())
  }

  def findContactByDLCId(dlcId: Sha256Digest): Future[Option[DLCContactDb]] = {
    val join =
      table.join(contactTable).on(_.contactId === _.address)
    safeDatabase
      .run(join.filter(_._1.dlcId === dlcId).result)
      .map(_.headOption.map(_._2))
  }

  def findDLCsByContactId(
      contactId: InetSocketAddress): Future[Vector[DLCDb]] = {
    val join = table.join(dlcTable).on(_.dlcId === _.dlcId)
    safeDatabase
      .runVec(join.filter(_._1.contactId === contactId).result)
      .map(_.map(_._2))
  }

  def listAll(): Future[Vector[DLCContactMapping]] = {
    val contactJoin = table.join(contactTable).on(_.contactId === _.address)
    val dlcJoin = dlcTable.joinLeft(contactJoin).on(_.dlcId === _._1.dlcId)
    safeDatabase
      .runVec(dlcJoin.result)
      .map(_.map(row => DLCContactMapping(row._1, row._2.map(_._2))))
  }

  override protected def findByPrimaryKeys(ids: Vector[Sha256Digest]): Query[
    Table[DLCContactMappingDb],
    DLCContactMappingDb,
    Seq] = {
    table.filter(_.dlcId.inSet(ids))
  }

  override protected def findAll(ts: Vector[DLCContactMappingDb]): Query[
    Table[_],
    DLCContactMappingDb,
    Seq] = findByPrimaryKeys(ts.map(_.dlcId))

  class DLCContactMappingTable(tag: Tag)
      extends Table[DLCContactMappingDb](tag,
                                         schemaName,
                                         "dlc_contact_mapping") {

    import profile.api._

    def dlcId: Rep[Sha256Digest] = column("dlc_id", O.PrimaryKey)

    def contactId: Rep[InetSocketAddress] = column("contact_id")

    def fkDLCId: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_dlc_contact_dlc_id",
                 sourceColumns = dlcId,
                 targetTableQuery = dlcTable)(_.dlcId)

    def fkContactId: ForeignKeyQuery[_, DLCContactDb] =
      foreignKey("fk_dlc_contact_contact_id",
                 sourceColumns = contactId,
                 targetTableQuery = contactTable)(_.address)

    def * =
      (dlcId,
       contactId) <> (DLCContactMappingDb.tupled, DLCContactMappingDb.unapply)
  }
}
