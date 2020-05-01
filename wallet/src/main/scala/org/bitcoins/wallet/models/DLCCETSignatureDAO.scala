package org.bitcoins.wallet.models

import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCCETSignatureDAO()(
    implicit val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUD[DLCCETSignatureDb, (Sha256DigestBE, Sha256DigestBE)]
    with SlickUtil[DLCCETSignatureDb, (Sha256DigestBE, Sha256DigestBE)] {
  import org.bitcoins.db.DbCommonsColumnMappers._
  import profile.api._

  override val table: TableQuery[DLCCETSignatureTable] =
    TableQuery[DLCCETSignatureTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCCETSignatureDb]): Future[Vector[DLCCETSignatureDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(ids: Vector[(
      Sha256DigestBE,
      Sha256DigestBE)]): Query[DLCCETSignatureTable, DLCCETSignatureDb, Seq] =
    table
      .filter(_.eventId.inSet(ids.map(_._1)))
      .filter(_.outcomeHash.inSet(ids.map(_._2)))

  override def findByPrimaryKey(id: (Sha256DigestBE, Sha256DigestBE)): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] = {
    table
      .filter(_.eventId === id._1)
      .filter(_.outcomeHash === id._2)
  }

  override def findAll(dlcs: Vector[DLCCETSignatureDb]): Query[
    DLCCETSignatureTable,
    DLCCETSignatureDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(sig => (sig.eventId, sig.outcomeHash)))

  def findByEventId(
      eventId: Sha256DigestBE): Future[Vector[DLCCETSignatureDb]] = {
    val q = table.filter(_.eventId === eventId)
    safeDatabase.run(q.result).map(_.toVector)
  }

  def findByEventId(eventId: Sha256Digest): Future[Vector[DLCCETSignatureDb]] =
    findByEventId(eventId.flip)

  class DLCCETSignatureTable(tag: Tag)
      extends Table[DLCCETSignatureDb](tag, "wallet_dlc_cet_sigs") {

    import org.bitcoins.db.DbCommonsColumnMappers._

    def eventId: Rep[Sha256DigestBE] = column("eventId")

    def outcomeHash: Rep[Sha256DigestBE] = column("outcomeHash")

    def signature: Rep[PartialSignature] = column("signature")

    private type DLCCETSignatureTuple =
      (Sha256DigestBE, Sha256DigestBE, PartialSignature)

    private val fromTuple: DLCCETSignatureTuple => DLCCETSignatureDb = {
      case (eventId, outcomeHash, signature) =>
        DLCCETSignatureDb(
          eventId,
          outcomeHash,
          signature
        )
    }

    private val toTuple: DLCCETSignatureDb => Option[DLCCETSignatureTuple] =
      dlc => Some((dlc.eventId, dlc.outcomeHash, dlc.signature))

    def * : ProvenShape[DLCCETSignatureDb] =
      (eventId, outcomeHash, signature) <> (fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc", sourceColumns = (eventId, outcomeHash))

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_eventId",
                 sourceColumns = eventId,
                 targetTableQuery = dlcTable)(_.eventId)
  }
}
