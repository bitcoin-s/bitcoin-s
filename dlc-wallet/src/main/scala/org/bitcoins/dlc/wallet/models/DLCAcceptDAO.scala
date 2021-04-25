package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.crypto.{ECPublicKey, Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.dlc.wallet.DLCAppConfig
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCAcceptDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: DLCAppConfig)
    extends CRUD[DLCAcceptDb, Sha256DigestBE]
    with SlickUtil[DLCAcceptDb, Sha256DigestBE] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCAcceptTable] = TableQuery[DLCAcceptTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(ts: Vector[DLCAcceptDb]): Future[Vector[DLCAcceptDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[DLCAcceptTable, DLCAcceptDb, Seq] =
    table.filter(_.paramHash.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[DLCAcceptTable, DLCAcceptDb, Seq] = {
    table
      .filter(_.paramHash === id)
  }

  override def findAll(
      dlcs: Vector[DLCAcceptDb]): Query[DLCAcceptTable, DLCAcceptDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.paramHash))

  def deleteByParamHash(paramHash: Sha256DigestBE): Future[Int] = {
    val q = table.filter(_.paramHash === paramHash)
    safeDatabase.run(q.delete)
  }

  def findByParamHash(
      paramHash: Sha256DigestBE): Future[Option[DLCAcceptDb]] = {
    val q = table.filter(_.paramHash === paramHash)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCAcceptDb] =>
        throw new RuntimeException(
          s"More than one DLCAccept per paramHash ($paramHash), got: $dlcs")
    }
  }

  def findByParamHash(paramHash: Sha256Digest): Future[Option[DLCAcceptDb]] =
    findByParamHash(paramHash.flip)

  class DLCAcceptTable(tag: Tag)
      extends Table[DLCAcceptDb](tag, "wallet_dlc_accepts") {

    def paramHash: Rep[Sha256DigestBE] = column("Param_hash", O.PrimaryKey)

    def tempContractId: Rep[Sha256Digest] =
      column("temp_contract_id", O.Unique)

    def fundingKey: Rep[ECPublicKey] = column("funding_key")

    def payoutAddress: Rep[BitcoinAddress] = column("payout_address")

    def payoutSerialId: Rep[UInt64] = column("payout_serial_id")

    def totalCollateral: Rep[CurrencyUnit] = column("total_collateral")

    def changeAddress: Rep[BitcoinAddress] = column("change_address")

    def changeSerialId: Rep[UInt64] = column("change_serial_id")

    def * : ProvenShape[DLCAcceptDb] =
      (paramHash,
       tempContractId,
       fundingKey,
       payoutAddress,
       payoutSerialId,
       totalCollateral,
       changeAddress,
       changeSerialId).<>(DLCAcceptDb.tupled, DLCAcceptDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_accept", sourceColumns = paramHash)

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_param_hash",
                 sourceColumns = paramHash,
                 targetTableQuery = dlcTable)(_.paramHash)

    def fkTempContractId: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_temp_contract_id",
                 sourceColumns = tempContractId,
                 targetTableQuery = dlcTable)(_.tempContractId)
  }
}
