package org.bitcoins.dlc.commons.oracle

import org.bitcoins.core.dlc.oracle.OracleAnnouncementDataDb
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.BaseEventDescriptor
import org.bitcoins.crypto.{SchnorrDigitalSignature, SchnorrPublicKey}
import org.bitcoins.db.{CRUDAutoInc, DbAppConfig}
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class OracleAnnouncementDataDAO()(implicit
    override val ec: ExecutionContext,
    override val appConfig: DbAppConfig)
    extends CRUDAutoInc[OracleAnnouncementDataDb] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)

  import mappers._
  import profile.api._

  override val table: TableQuery[OracleAnnouncementsTable] =
    TableQuery[OracleAnnouncementsTable]

  def findByAnnouncementPublicKey(
      publicKey: SchnorrPublicKey): Future[Vector[OracleAnnouncementDataDb]] = {
    val query = table.filter(_.announcementPublicKey === publicKey)

    safeDatabase.runVec(query.result)
  }

  def findByAnnouncementSignatures(
      signatures: Vector[SchnorrDigitalSignature]): Future[
    Vector[OracleAnnouncementDataDb]] = {
    val query = table.filter(_.announcementSignature.inSet(signatures))

    safeDatabase
      .runVec(query.result)
  }

  def findByIds(ids: Vector[Long]): Future[Vector[OracleAnnouncementDataDb]] = {
    safeDatabase.run(findByIdsAction(ids))
  }

  def findByIdsAction(ids: Vector[Long]): DBIOAction[
    Vector[OracleAnnouncementDataDb],
    NoStream,
    Effect.Read] = {
    table.filter(_.id.inSet(ids)).result.map(_.toVector)
  }

  def findByIdAction(id: Long): DBIOAction[
    Option[OracleAnnouncementDataDb],
    NoStream,
    Effect.Read] = {
    table
      .filter(_.id === id)
      .result
      .map(_.headOption)
  }

  def findById(id: Long): Future[Option[OracleAnnouncementDataDb]] = {
    findByIds(Vector(id)).map(_.headOption)
  }

  def findByEventNameAction(eventName: String): DBIOAction[
    Vector[OracleAnnouncementDataDb],
    NoStream,
    Effect.Read] = {
    val action = table
      .filter(_.eventId === eventName)
      .result
      .map(_.toVector)
    action
  }

  def findByEventName(
      eventName: String): Future[Vector[OracleAnnouncementDataDb]] = {
    val action = findByEventNameAction(eventName)
    safeDatabase.run(action)
  }

  def deleteByAnnouncementIdAction(
      announcementId: Long): DBIOAction[Int, NoStream, Effect.Write] = {
    table.filter(_.id === announcementId).delete
  }

  class OracleAnnouncementsTable(tag: Tag)
      extends TableAutoInc[OracleAnnouncementDataDb](
        tag,
        schemaName,
        "oracle_announcement_data") {

    def announcementSignature: Rep[SchnorrDigitalSignature] = column(
      "announcement_signature")

    def announcementPublicKey: Rep[SchnorrPublicKey] = column("pub_key")

    def attestationPublicKey: Rep[SchnorrPublicKey] = column("signing_pub_key")

    def eventId: Rep[String] = column("event_id")

    def eventDescriptor: Rep[BaseEventDescriptor] = column("event_descriptor")

    def eventMaturity: Rep[UInt32] = column("event_maturity")

    override def * : ProvenShape[OracleAnnouncementDataDb] =
      (id.?,
       announcementSignature,
       announcementPublicKey,
       attestationPublicKey,
       eventId,
       eventDescriptor,
       eventMaturity)
        .<>(OracleAnnouncementDataDb.tupled, OracleAnnouncementDataDb.unapply)
  }
}
