package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.tlv.ContractInfoV0TLV
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCOfferDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCOfferDb, Sha256DigestBE]
    with SlickUtil[DLCOfferDb, Sha256DigestBE] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCOfferTable] = TableQuery[DLCOfferTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(ts: Vector[DLCOfferDb]): Future[Vector[DLCOfferDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[DLCOfferTable, DLCOfferDb, Seq] =
    table.filter(_.paramHash.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[DLCOfferTable, DLCOfferDb, Seq] = {
    table
      .filter(_.paramHash === id)
  }

  override def findAll(
      dlcs: Vector[DLCOfferDb]): Query[DLCOfferTable, DLCOfferDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.paramHash))

  def deleteByParamHash(paramHash: Sha256DigestBE): Future[Int] = {
    val q = table.filter(_.paramHash === paramHash)
    safeDatabase.run(q.delete)
  }

  def findByParamHash(paramHash: Sha256DigestBE): Future[Option[DLCOfferDb]] = {
    val q = table.filter(_.paramHash === paramHash)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCOfferDb] =>
        throw new RuntimeException(
          s"More than one DLCOffer per paramHash ($paramHash), got: $dlcs")
    }
  }

  def findByParamHash(paramHash: Sha256Digest): Future[Option[DLCOfferDb]] =
    findByParamHash(paramHash.flip)

  class DLCOfferTable(tag: Tag)
      extends Table[DLCOfferDb](tag, "wallet_dlc_offers") {

    def paramHash: Rep[Sha256DigestBE] = column("param_hash", O.Unique)

    def tempContractId: Rep[Sha256Digest] =
      column("temp_contract_id", O.Unique)

    def contractInfoTLV: Rep[ContractInfoV0TLV] = column("contract_info")

    def contractMaturity: Rep[BlockTimeStamp] = column("contract_maturity")

    def contractTimeout: Rep[BlockTimeStamp] = column("contract_timeout")

    def fundingKey: Rep[ECPublicKey] = column("funding_key")

    def payoutAddress: Rep[BitcoinAddress] = column("payout_address")

    def payoutSerialId: Rep[UInt64] = column("payout_serial_id")

    def totalCollateral: Rep[CurrencyUnit] = column("total_collateral")

    def feeRate: Rep[SatoshisPerVirtualByte] = column("fee_rate")

    def changeAddress: Rep[BitcoinAddress] = column("change_address")

    def changeSerialId: Rep[UInt64] = column("change_serial_id")

    def fundOutputSerialId: Rep[UInt64] = column("fund_output_serial_id")

    def * : ProvenShape[DLCOfferDb] =
      (paramHash,
       tempContractId,
       contractInfoTLV,
       contractMaturity,
       contractTimeout,
       fundingKey,
       payoutAddress,
       payoutSerialId,
       totalCollateral,
       feeRate,
       changeAddress,
       changeSerialId,
       fundOutputSerialId).<>(DLCOfferDb.tupled, DLCOfferDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_offer", sourceColumns = paramHash)

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_paramHash",
                 sourceColumns = paramHash,
                 targetTableQuery = dlcTable)(_.paramHash)

    def fkTempContractId: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_temp_contract_id",
                 sourceColumns = tempContractId,
                 targetTableQuery = dlcTable)(_.tempContractId)
  }
}
