package org.bitcoins.dlc.oracle.storage

import org.bitcoins.crypto.{SchnorrNonce, Sha256Digest}
import org.bitcoins.db.{AppConfig, CRUD, DbCommonsColumnMappers, SlickUtil}
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class EventOutcomeDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: AppConfig)
    extends CRUD[EventOutcomeDb, (SchnorrNonce, String)]
    with SlickUtil[EventOutcomeDb, (SchnorrNonce, String)] {

  import profile.api._

  private val mappers = new DbCommonsColumnMappers(profile)

  import mappers._

  override val table: TableQuery[EventOutcomeTable] =
    TableQuery[EventOutcomeTable]

  private lazy val eventTable: TableQuery[EventDAO#EventTable] =
    EventDAO().table

  override def createAll(
      ts: Vector[EventOutcomeDb]): Future[Vector[EventOutcomeDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(ids: Vector[
    (SchnorrNonce, String)]): Query[EventOutcomeTable, EventOutcomeDb, Seq] =
    table
      .filter(_.nonce.inSet(ids.map(_._1)))
      .filter(_.message.inSet(ids.map(_._2)))

  override protected def findAll(ts: Vector[EventOutcomeDb]): Query[
    EventOutcomeTable,
    EventOutcomeDb,
    Seq] = {
    val ids = ts.map(t => (t.nonce, t.message))
    findByPrimaryKeys(ids)
  }

  def findByNonce(nonce: SchnorrNonce): Future[Vector[EventOutcomeDb]] = {
    val query = table.filter(_.nonce === nonce)

    safeDatabase.runVec(query.result.transactionally)
  }

  class EventOutcomeTable(tag: Tag)
      extends Table[EventOutcomeDb](tag, schemaName, "event_outcomes") {

    def nonce: Rep[SchnorrNonce] = column("nonce")

    def message: Rep[String] = column("message")

    def hashedMessage: Rep[Sha256Digest] = column("hashed_message")

    def * : ProvenShape[EventOutcomeDb] =
      (nonce,
       message,
       hashedMessage) <> (EventOutcomeDb.tupled, EventOutcomeDb.unapply)

    def fk: ForeignKeyQuery[_, EventDb] = {
      foreignKey("fk_nonce",
                 sourceColumns = nonce,
                 targetTableQuery = eventTable)(_.nonce)
    }
  }
}
