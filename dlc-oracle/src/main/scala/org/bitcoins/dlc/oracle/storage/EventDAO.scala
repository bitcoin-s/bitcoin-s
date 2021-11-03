package org.bitcoins.dlc.oracle.storage

import org.bitcoins.core.api.dlcoracle.db.{EventDb, RValueDb}
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv.{
  BaseEventDescriptor,
  BaseOracleAnnouncement,
  OracleAnnouncementV0TLV,
  OracleAnnouncementV1TLV
}
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, DbCommonsColumnMappers, SlickUtil}
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import slick.lifted.{ForeignKeyQuery, ProvenShape}

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

case class EventDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DLCOracleAppConfig)
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
    val query = table.filter(_.attestationOpt.isEmpty)

    safeDatabase.runVec(query.result)
  }

  def getCompletedEvents: Future[Vector[EventDb]] = {
    val query = table.filter(_.attestationOpt.isDefined)

    safeDatabase.runVec(query.result)
  }

  def findByEventName(name: String): Future[Vector[EventDb]] = {
    val query = table.filter(_.eventName === name)

    safeDatabase.runVec(query.result)
  }

  def findByEventDescriptor(
      descriptorTLV: BaseEventDescriptor): Future[Vector[EventDb]] = {
    val query = table.filter(_.eventDescriptorTLV === descriptorTLV)

    safeDatabase.runVec(query.result)
  }

  def findByAnnouncement(
      announcement: BaseOracleAnnouncement): Future[Vector[EventDb]] = {
    val nonces = announcement match {
      case v0: OracleAnnouncementV0TLV =>
        v0.eventTLV.nonces
      case v1: OracleAnnouncementV1TLV =>
        v1.metadata.attestations.nonces
    }

    val query = table.filter(_.nonce.inSet(nonces))
    safeDatabase.runVec(query.result)
  }

  def findDifferentPublicKey(key: SchnorrPublicKey): Future[Vector[EventDb]] = {
    val query = table.filterNot(_.pubkey === key)

    safeDatabase.runVec(query.result)
  }

  class EventTable(tag: Tag) extends Table[EventDb](tag, schemaName, "events") {

    def nonce: Rep[SchnorrNonce] = column("nonce", O.PrimaryKey)

    def pubkey: Rep[SchnorrPublicKey] = column("pubkey")

    def nonceIndex: Rep[Int] = column("nonce_index")

    def eventName: Rep[String] = column("event_name")

    def numOutcomes: Rep[Long] = column("num_outcomes")

    def signingVersion: Rep[SigningVersion] = column("signing_version")

    def maturationTime: Rep[Instant] = column("maturation_time")

    def attestationOpt: Rep[Option[FieldElement]] = column("attestation")

    def outcomeOpt: Rep[Option[String]] = column("outcome")

    def announcementSignature: Rep[SchnorrDigitalSignature] =
      column("announcement_signature")

    def eventDescriptorTLV: Rep[BaseEventDescriptor] =
      column("event_descriptor_tlv")

    def * : ProvenShape[EventDb] =
      (nonce,
       pubkey,
       nonceIndex,
       eventName,
       numOutcomes,
       signingVersion,
       maturationTime,
       attestationOpt,
       outcomeOpt,
       announcementSignature,
       eventDescriptorTLV).<>(EventDb.tupled, EventDb.unapply)

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
