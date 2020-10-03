package org.bitcoins.dlc.oracle.storage

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.crypto._
import org.bitcoins.db.{AppConfig, CRUD, DbCommonsColumnMappers, SlickUtil}
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class EventDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: AppConfig)
    extends CRUD[EventDb, SchnorrNonce]
    with SlickUtil[EventDb, SchnorrNonce] {

  import profile.api._

  private val mappers = new DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[EventTable] = TableQuery[EventTable]

  private lazy val rValueTable: TableQuery[RValueDAO#RValueTable] =
    RValueDAO().table

  override def createAll(ts: Vector[EventDb]): Future[Vector[EventDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[SchnorrNonce]): Query[EventTable, EventDb, Seq] =
    table.filter(_.nonce.inSet(ids))

  override protected def findAll(
      ts: Vector[EventDb]): Query[EventTable, EventDb, Seq] =
    findByPrimaryKeys(ts.map(_.nonce))

  def getPendingEvents: Future[Vector[EventDb]] = {
    findAll().map(_.filter(_.attestationOpt.isEmpty))
  }

  class EventTable(tag: Tag) extends Table[EventDb](tag, schemaName, "events") {

    def nonce: Rep[SchnorrNonce] = column("nonce", O.PrimaryKey)

    def pubkey: Rep[SchnorrPublicKey] = column("pubkey")

    def eventName: Rep[String] = column("event_name", O.Unique)

    def numOutcomes: Rep[Long] = column("num_outcomes")

    def signingVersion: Rep[SigningVersion] = column("signing_version")

    def maturationTime: Rep[Instant] = column("maturation_time")

    def attestationOpt: Rep[Option[FieldElement]] = column("attestation")

    def * : ProvenShape[EventDb] =
      (nonce,
       pubkey,
       eventName,
       numOutcomes,
       signingVersion,
       maturationTime,
       attestationOpt) <> (EventDb.tupled, EventDb.unapply)

    def fk: ForeignKeyQuery[_, RValueDb] = {
      foreignKey("fk_nonce",
                 sourceColumns = nonce,
                 targetTableQuery = rValueTable)(_.nonce)
    }

    def fkLabel: ForeignKeyQuery[_, RValueDb] = {
      foreignKey("fk_label",
                 sourceColumns = eventName,
                 targetTableQuery = rValueTable)(_.eventName)
    }
  }
}
