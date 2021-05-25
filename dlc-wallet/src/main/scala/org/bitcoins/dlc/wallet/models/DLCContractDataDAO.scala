package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BlockTimeStamp
import org.bitcoins.core.protocol.tlv.{ContractDescriptorTLV, OracleParamsV0TLV}
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted._

import scala.concurrent.{ExecutionContext, Future}

case class DLCContractDataDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCContractDataDb, Sha256Digest]
    with SlickUtil[DLCContractDataDb, Sha256Digest] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCContractDataTable] =
    TableQuery[DLCContractDataTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(
      ts: Vector[DLCContractDataDb]): Future[Vector[DLCContractDataDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(ids: Vector[Sha256Digest]): Query[
    DLCContractDataTable,
    DLCContractDataDb,
    Seq] =
    table.filter(_.dlcId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256Digest): Query[DLCContractDataTable, DLCContractDataDb, Seq] = {
    table
      .filter(_.dlcId === id)
  }

  override def findAll(dlcs: Vector[DLCContractDataDb]): Query[
    DLCContractDataTable,
    DLCContractDataDb,
    Seq] =
    findByPrimaryKeys(dlcs.map(_.dlcId))

  def deleteByDLCId(dlcId: Sha256Digest): Future[Int] = {
    val q = table.filter(_.dlcId === dlcId)
    safeDatabase.run(q.delete)
  }

  def findByDLCId(dlcId: Sha256Digest): Future[Option[DLCContractDataDb]] = {
    val q = table.filter(_.dlcId === dlcId)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCContractDataDb] =>
        throw new RuntimeException(
          s"More than one DLC per dlcId (${dlcId.hex}), got: $dlcs")
    }
  }

  class DLCContractDataTable(tag: Tag)
      extends Table[DLCContractDataDb](tag, schemaName, "contract_data") {

    def dlcId: Rep[Sha256Digest] = column("dlc_id", O.PrimaryKey)

    def oracleThreshold: Rep[Int] = column("oracle_threshold")

    def oracleParamsOpt: Rep[Option[OracleParamsV0TLV]] = column(
      "oracle_params")

    def contractDescriptor: Rep[ContractDescriptorTLV] = column(
      "contract_descriptor")

    def contractMaturity: Rep[BlockTimeStamp] = column("contract_maturity")

    def contractTimeout: Rep[BlockTimeStamp] = column("contract_timeout")

    def totalCollateral: Rep[CurrencyUnit] = column("total_collateral")

    def * : ProvenShape[DLCContractDataDb] =
      (dlcId,
       oracleThreshold,
       oracleParamsOpt,
       contractDescriptor,
       contractMaturity,
       contractTimeout,
       totalCollateral).<>(DLCContractDataDb.tupled, DLCContractDataDb.unapply)

    def fk =
      foreignKey("fk_dlc_id",
                 sourceColumns = dlcId,
                 targetTableQuery = dlcTable)(_.dlcId)
  }
}
