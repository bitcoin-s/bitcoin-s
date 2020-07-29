package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.crypto._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class OracleAnnouncementDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[OracleAnnouncementDb, OracleAnnouncementTLV]
    with SlickUtil[OracleAnnouncementDb, OracleAnnouncementTLV] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[OracleAnnouncementsTable] =
    TableQuery[OracleAnnouncementsTable]

  override def createAll(
      ts: Vector[OracleAnnouncementDb]): Future[Vector[OracleAnnouncementDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  /** Finds the rows that correlate to the given primary keys */
  override protected def findByPrimaryKeys(
      ids: Vector[OracleAnnouncementTLV]): Query[
    Table[OracleAnnouncementDb],
    OracleAnnouncementDb,
    Seq] =
    table.filter(_.announcement.inSet(ids))

  override protected def findAll(
      ts: Vector[OracleAnnouncementDb]): profile.api.Query[
    profile.api.Table[_],
    OracleAnnouncementDb,
    Seq] = table.filter(_.announcement.inSet(ts.map(_.announcement)))

  def findByPublicKey(
      publicKey: SchnorrPublicKey): Future[Vector[OracleAnnouncementDb]] = {
    val query = table.filter(_.publicKey === publicKey)

    safeDatabase.runVec(query.result)
  }

  class OracleAnnouncementsTable(tag: Tag)
      extends Table[OracleAnnouncementDb](tag,
                                          schemaName,
                                          "oracle_announcements") {

    def announcement: Rep[OracleAnnouncementTLV] =
      column("announcement", O.PrimaryKey)
    def publicKey: Rep[SchnorrPublicKey] = column("pub_key")

    def * : ProvenShape[OracleAnnouncementDb] =
      (announcement, publicKey)
        .<>(OracleAnnouncementDb.tupled, OracleAnnouncementDb.unapply)
  }
}
