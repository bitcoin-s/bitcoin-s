package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.hd.{HDAccount, HDChainType}
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.models.DLCState
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import scodec.bits.ByteVector
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class DLCDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCDb, Sha256Digest]
    with SlickUtil[DLCDb, Sha256Digest] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCTable] = TableQuery[DLCTable]

  override def createAll(ts: Vector[DLCDb]): Future[Vector[DLCDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256Digest]): Query[DLCTable, DLCDb, Seq] =
    table.filter(_.dlcId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256Digest): Query[DLCTable, DLCDb, Seq] = {
    table
      .filter(_.dlcId === id)
  }

  override def findAll(dlcs: Vector[DLCDb]): Query[DLCTable, DLCDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.dlcId))

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    val q = table.filter(_.dlcId === dlcId)
    safeDatabase.run(q.delete)
  }

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

  def findByDLCId(dlcId: Sha256Digest): Future[Option[DLCDb]] = {
    val q = table.filter(_.dlcId === dlcId)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCDb] =>
        throw new RuntimeException(
          s"More than one DLC per dlcId (${dlcId.hex}), got: $dlcs")
    }
  }

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

  def findByFundingTxId(txId: DoubleSha256DigestBE): Future[Vector[DLCDb]] = {
    val q = table.filter(_.fundingTxIdOpt === txId)

    safeDatabase.runVec(q.result)
  }

  class DLCTable(tag: Tag)
      extends Table[DLCDb](tag, schemaName, "global_dlc_data") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id", O.PrimaryKey)

    def tempContractId: Rep[Sha256Digest] =
      column("temp_contract_id", O.Unique)

    def contractId: Rep[Option[ByteVector]] =
      column("contract_id", O.Unique)

    def protocolVersion: Rep[Int] = column("protocol_version")

    def state: Rep[DLCState] = column("state")

    def isInitiator: Rep[Boolean] = column("is_initiator")

    def account: Rep[HDAccount] = column("account")

    def changeIndex: Rep[HDChainType] = column("change_index")

    def keyIndex: Rep[Int] = column("key_index")

    def feeRate: Rep[SatoshisPerVirtualByte] = column("fee_rate")

    def fundOutputSerialId: Rep[UInt64] = column("fund_output_serial_id")

    def fundingOutPointOpt: Rep[Option[TransactionOutPoint]] =
      column("funding_outpoint")

    def fundingTxIdOpt: Rep[Option[DoubleSha256DigestBE]] =
      column("funding_tx_id")

    def closingTxIdOpt: Rep[Option[DoubleSha256DigestBE]] =
      column("closing_tx_id")

    def aggregateSignatureOpt: Rep[Option[SchnorrDigitalSignature]] = column(
      "aggregate_signature")

    def * : ProvenShape[DLCDb] =
      (dlcId,
       tempContractId,
       contractId,
       protocolVersion,
       state,
       isInitiator,
       account,
       changeIndex,
       keyIndex,
       feeRate,
       fundOutputSerialId,
       fundingOutPointOpt,
       fundingTxIdOpt,
       closingTxIdOpt,
       aggregateSignatureOpt).<>(DLCDb.tupled, DLCDb.unapply)
  }
}
