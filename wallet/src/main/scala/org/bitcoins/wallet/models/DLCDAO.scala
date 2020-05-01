package org.bitcoins.wallet.models

import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{
  SchnorrDigitalSignature,
  Sha256Digest,
  Sha256DigestBE
}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCDAO()(
    implicit val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUD[DLCDb, Sha256DigestBE]
    with SlickUtil[DLCDb, Sha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._
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

    import org.bitcoins.db.DbCommonsColumnMappers._

    def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

    def isInitiator: Rep[Boolean] = column("isInitiator")

    def account: Rep[HDAccount] = column("account")

    def keyIndex: Rep[Int] = column("keyIndex")

    def initiatorRefundSigOpt: Rep[Option[PartialSignature]] =
      column("initiatorRefundSig")

    def oracleSigOpt: Rep[Option[SchnorrDigitalSignature]] = column("oracleSig")

    private type DLCTuple = (
        Sha256DigestBE,
        Boolean,
        HDAccount,
        Int,
        Option[PartialSignature],
        Option[SchnorrDigitalSignature])

    private val fromTuple: DLCTuple => DLCDb = {
      case (eventId,
            isInitiator,
            account,
            keyIndex,
            initiatorRefundSigOpt,
            oracleSigOpt) =>
        DLCDb(
          eventId,
          isInitiator,
          account,
          keyIndex,
          initiatorRefundSigOpt,
          oracleSigOpt
        )
    }

    private val toTuple: DLCDb => Option[DLCTuple] = dlc =>
      Some(
        (dlc.eventId,
         dlc.isInitiator,
         dlc.account,
         dlc.keyIndex,
         dlc.refundSigOpt,
         dlc.oracleSigOpt))

    def * : ProvenShape[DLCDb] =
      (eventId,
       isInitiator,
       account,
       keyIndex,
       initiatorRefundSigOpt,
       oracleSigOpt) <> (fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc", sourceColumns = eventId)
  }
}
