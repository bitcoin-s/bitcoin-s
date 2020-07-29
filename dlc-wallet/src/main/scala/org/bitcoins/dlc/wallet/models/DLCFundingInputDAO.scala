package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.crypto.{Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCFundingInputDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
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

  def deleteByParamHash(paramHash: Sha256DigestBE): Future[Int] = {
    val q = table.filter(_.paramHash === paramHash)
    safeDatabase.run(q.delete)
  }

  def findByParamHash(
      paramHash: Sha256DigestBE): Future[Vector[DLCFundingInputDb]] = {
    val q = table.filter(_.paramHash === paramHash)

    safeDatabase.run(q.result).map(_.toVector)
  }

  def findByParamHash(
      paramHash: Sha256Digest): Future[Vector[DLCFundingInputDb]] =
    findByParamHash(paramHash.flip)

  def findByParamHash(
      paramHash: Sha256DigestBE,
      isInitiator: Boolean): Future[Vector[DLCFundingInputDb]] = {
    val q = table
      .filter(_.paramHash === paramHash)
      .filter(_.isInitiator === isInitiator)

    safeDatabase.run(q.result).map(_.toVector)
  }

  def findByParamHash(
      paramHash: Sha256Digest,
      isInitiator: Boolean): Future[Vector[DLCFundingInputDb]] =
    findByParamHash(paramHash.flip, isInitiator)

  class DLCFundingInputsTable(tag: Tag)
      extends Table[DLCFundingInputDb](tag, "wallet_dlc_funding_inputs") {

    def paramHash: Rep[Sha256DigestBE] = column("param_hash")

    def inputSerialId: Rep[UInt64] = column("input_serial_id")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def outPoint: Rep[TransactionOutPoint] = column("out_point", O.Unique)

    def output: Rep[TransactionOutput] = column("output")

    def redeemScriptOpt: Rep[Option[ScriptPubKey]] = column("redeem_script_opt")

    def witnessScriptOpt: Rep[Option[ScriptWitness]] =
      column("witness_script_opt")

    def * : ProvenShape[DLCFundingInputDb] =
      (paramHash,
       isInitiator,
       inputSerialId,
       outPoint,
       output,
       redeemScriptOpt,
       witnessScriptOpt).<>(DLCFundingInputDb.tupled, DLCFundingInputDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_input", sourceColumns = outPoint)

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_param_hash",
                 sourceColumns = paramHash,
                 targetTableQuery = dlcTable)(_.paramHash)
  }
}
