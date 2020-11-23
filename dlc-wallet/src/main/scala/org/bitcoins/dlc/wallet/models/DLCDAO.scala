package org.bitcoins.dlc.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCState
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.tlv.DLCOutcomeType
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  SchnorrDigitalSignature,
  Sha256Digest,
  Sha256DigestBE
}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import scodec.bits.ByteVector
import slick.lifted.{PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
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
    table.filter(_.paramHash.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[DLCTable, DLCDb, Seq] = {
    table
      .filter(_.paramHash === id)
  }

  override def findAll(dlcs: Vector[DLCDb]): Query[DLCTable, DLCDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.paramHash))

  def findByTempContractId(
      tempContractId: Sha256Digest): Future[Option[DLCDb]] = {
    val q = table.filter(_.tempContractId === tempContractId)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCDb] =>
        throw new RuntimeException(
          s"More than one DLC per tempContractId (${tempContractId.hex}), got: $dlcs")
    }
  }

  def findByTempContractId(
      tempContractId: Sha256DigestBE): Future[Option[DLCDb]] =
    findByTempContractId(tempContractId.flip)

  def findByContractId(contractId: ByteVector): Future[Option[DLCDb]] = {
    val q = table.filter(_.contractId === contractId)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCDb] =>
        throw new RuntimeException(
          s"More than one DLC per contractId (${contractId.toHex}), got: $dlcs")
    }
  }

  def findByParamHash(paramHash: Sha256DigestBE): Future[Option[DLCDb]] = {
    val q = table.filter(_.paramHash === paramHash)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCDb] =>
        throw new RuntimeException(
          s"More than one DLC per paramHash (${paramHash.hex}), got: $dlcs")
    }
  }

  def findByParamHash(paramHash: Sha256Digest): Future[Option[DLCDb]] =
    findByParamHash(paramHash.flip)

  def findByFundingOutPoint(
      outPoint: TransactionOutPoint): Future[Option[DLCDb]] = {
    val q = table.filter(_.fundingOutPointOpt === outPoint)

    safeDatabase.run(q.result).map(_.headOption)
  }

  def findByFundingOutPoints(
      outPoints: Vector[TransactionOutPoint]): Future[Vector[DLCDb]] = {
    val q = table.filter(_.fundingOutPointOpt.inSet(outPoints))

    safeDatabase.runVec(q.result)
  }

  def findByFundingTxIds(
      txIds: Vector[DoubleSha256DigestBE]): Future[Vector[DLCDb]] = {
    val q = table.filter(_.fundingTxIdOpt.inSet(txIds))

    safeDatabase.runVec(q.result)
  }

  class DLCTable(tag: Tag) extends Table[DLCDb](tag, "wallet_dlcs") {

    def paramHash: Rep[Sha256DigestBE] = column("param_hash", O.PrimaryKey)

    def tempContractId: Rep[Sha256Digest] =
      column("temp_contract_id", O.Unique)

    def contractId: Rep[Option[ByteVector]] =
      column("contract_id", O.Unique)

    def state: Rep[DLCState] = column("state")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def account: Rep[HDAccount] = column("account")

    def keyIndex: Rep[Int] = column("key_index")

    def oracleSigsOpt: Rep[Option[Vector[SchnorrDigitalSignature]]] =
      column("oracle_sigs")

    def fundingOutPointOpt: Rep[Option[TransactionOutPoint]] =
      column("funding_outpoint")

    def fundingTxIdOpt: Rep[Option[DoubleSha256DigestBE]] =
      column("funding_tx_id")

    def closingTxIdOpt: Rep[Option[DoubleSha256DigestBE]] =
      column("closing_tx_id")

    def outcomeOpt: Rep[Option[DLCOutcomeType]] = column("outcome")

    def * : ProvenShape[DLCDb] =
      (paramHash,
       tempContractId,
       contractId,
       state,
       isInitiator,
       account,
       keyIndex,
       oracleSigsOpt,
       fundingOutPointOpt,
       fundingTxIdOpt,
       closingTxIdOpt,
       outcomeOpt).<>(DLCDb.tupled, DLCDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc", sourceColumns = paramHash)
  }
}
