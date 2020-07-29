package org.bitcoins.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCState
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.crypto.{
  SchnorrDigitalSignature,
  Sha256Digest,
  Sha256DigestBE
}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUD[DLCDb, Sha256DigestBE]
    with SlickUtil[DLCDb, Sha256DigestBE] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCTable] = TableQuery[DLCTable]

  override def createAll(ts: Vector[DLCDb]): Future[Vector[DLCDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[DLCTable, DLCDb, Seq] =
    table.filter(_.eventId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[DLCTable, DLCDb, Seq] = {
    table
      .filter(_.eventId === id)
  }

  override def findAll(dlcs: Vector[DLCDb]): Query[DLCTable, DLCDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.eventId))

  def findByEventId(eventId: Sha256DigestBE): Future[Option[DLCDb]] = {
    val q = table.filter(_.eventId === eventId)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCDb] =>
        throw new RuntimeException(
          s"More than one DLC per eventId ($eventId), got: $dlcs")
    }
  }

  def findByEventId(eventId: Sha256Digest): Future[Option[DLCDb]] =
    findByEventId(eventId.flip)

  class DLCTable(tag: Tag) extends Table[DLCDb](tag, "wallet_dlcs") {

    def eventId: Rep[Sha256DigestBE] = column("event_id", O.Unique)

    def state: Rep[DLCState] = column("state")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def account: Rep[HDAccount] = column("account")

    def keyIndex: Rep[Int] = column("key_index")

    def oracleSigOpt: Rep[Option[SchnorrDigitalSignature]] =
      column("oracle_sig")

    def * : ProvenShape[DLCDb] =
      (eventId,
       state,
       isInitiator,
       account,
       keyIndex,
       oracleSigOpt) <> (DLCDb.tupled, DLCDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc", sourceColumns = eventId)
  }
}
