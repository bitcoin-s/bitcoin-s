package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.EventDescriptorTLV
import org.bitcoins.crypto._
import org.bitcoins.db._
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.ProvenShape

import scala.concurrent.{ExecutionContext, Future}

case class OracleAnnouncementDataDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUDAutoInc[OracleAnnouncementDataDb] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[OracleAnnouncementsTable] =
    TableQuery[OracleAnnouncementsTable]

  def findByPublicKey(
      publicKey: SchnorrPublicKey): Future[Vector[OracleAnnouncementDataDb]] = {
    val query = table.filter(_.publicKey === publicKey)

    safeDatabase.runVec(query.result)
  }

  def findByIds(ids: Vector[Long]): Future[Vector[OracleAnnouncementDataDb]] = {
    val query = table.filter(_.id.inSet(ids))

    safeDatabase.runVec(query.result)
  }

  class OracleAnnouncementsTable(tag: Tag)
      extends TableAutoInc[OracleAnnouncementDataDb](
        tag,
        schemaName,
        "oracle_announcement_data") {

    def announcementSignature: Rep[SchnorrDigitalSignature] = column(
      "announcement_signature")

    def publicKey: Rep[SchnorrPublicKey] = column("pub_key")

    def signingPublicKey: Rep[SchnorrPublicKey] = column("signing_pub_key")

    def eventId: Rep[String] = column("event_id")

    def eventDescriptor: Rep[EventDescriptorTLV] = column("event_descriptor")

    def eventMaturity: Rep[UInt32] = column("event_maturity")

    override def * : ProvenShape[OracleAnnouncementDataDb] =
      (id.?,
       announcementSignature,
       publicKey,
       signingPublicKey,
       eventId,
       eventDescriptor,
       eventMaturity)
        .<>(OracleAnnouncementDataDb.tupled, OracleAnnouncementDataDb.unapply)
  }
}
