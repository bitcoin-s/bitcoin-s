package org.bitcoins.wallet.models

import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCFundingInputDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUD[DLCFundingInputDb, TransactionOutPoint]
    with SlickUtil[DLCFundingInputDb, TransactionOutPoint] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCFundingInputsTable] =
    TableQuery[DLCFundingInputsTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCFundingInputDb]): Future[Vector[DLCFundingInputDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      outPoints: Vector[TransactionOutPoint]): Query[
    DLCFundingInputsTable,
    DLCFundingInputDb,
    Seq] =
    table.filter(_.outPoint.inSet(outPoints))

  override def findByPrimaryKey(outPoint: TransactionOutPoint): Query[
    DLCFundingInputsTable,
    DLCFundingInputDb,
    Seq] = {
    table
      .filter(_.outPoint === outPoint)
  }

  override def findAll(dlcs: Vector[DLCFundingInputDb]): Query[
    DLCFundingInputsTable,
    DLCFundingInputDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(_.outPoint))

  def findByEventId(
      eventId: Sha256DigestBE): Future[Vector[DLCFundingInputDb]] = {
    val q = table.filter(_.eventId === eventId)

    safeDatabase.run(q.result).map(_.toVector)
  }

  def findByEventId(eventId: Sha256Digest): Future[Vector[DLCFundingInputDb]] =
    findByEventId(eventId.flip)

  def findByEventId(
      eventId: Sha256DigestBE,
      isInitiator: Boolean): Future[Vector[DLCFundingInputDb]] = {
    val q = table
      .filter(_.eventId === eventId)
      .filter(_.isInitiator === isInitiator)

    safeDatabase.run(q.result).map(_.toVector)
  }

  def findByEventId(
      eventId: Sha256Digest,
      isInitiator: Boolean): Future[Vector[DLCFundingInputDb]] =
    findByEventId(eventId.flip, isInitiator)

  class DLCFundingInputsTable(tag: Tag)
      extends Table[DLCFundingInputDb](tag, "wallet_dlc_funding_inputs") {

    def eventId: Rep[Sha256DigestBE] = column("event_id")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def outPoint: Rep[TransactionOutPoint] = column("out_point", O.Unique)

    def output: Rep[TransactionOutput] = column("output")

    def redeemScriptOpt: Rep[Option[ScriptPubKey]] = column("redeem_script_opt")

    def witnessScriptOpt: Rep[Option[ScriptWitness]] =
      column("witness_script_opt")

    def sigs: Rep[Vector[PartialSignature]] = column("sigs")

    def * : ProvenShape[DLCFundingInputDb] =
      (eventId,
       isInitiator,
       outPoint,
       output,
       redeemScriptOpt,
       witnessScriptOpt,
       sigs) <> (DLCFundingInputDb.tupled, DLCFundingInputDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_input", sourceColumns = outPoint)

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_event_id",
                 sourceColumns = eventId,
                 targetTableQuery = dlcTable)(_.eventId)
  }
}
