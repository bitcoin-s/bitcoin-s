package org.bitcoins.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.crypto.{ECPublicKey, Sha256Digest, Sha256DigestBE}
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCAcceptDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
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
    table.filter(_.eventId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[DLCAcceptTable, DLCAcceptDb, Seq] = {
    table
      .filter(_.eventId === id)
  }

  override def findAll(
      dlcs: Vector[DLCAcceptDb]): Query[DLCAcceptTable, DLCAcceptDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.eventId))

  def findByEventId(eventId: Sha256DigestBE): Future[Option[DLCAcceptDb]] = {
    val q = table.filter(_.eventId === eventId)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCAcceptDb] =>
        throw new RuntimeException(
          s"More than one DLCAccept per eventId ($eventId), got: $dlcs")
    }
  }

  def findByEventId(eventId: Sha256Digest): Future[Option[DLCAcceptDb]] =
    findByEventId(eventId.flip)

  class DLCAcceptTable(tag: Tag)
      extends Table[DLCAcceptDb](tag, "wallet_dlc_accepts") {

    def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

    def fundingKey: Rep[ECPublicKey] = column("fundingKey")

    def toLocalCETKey: Rep[ECPublicKey] = column("toLocalCETKey")

    def finalAddress: Rep[BitcoinAddress] = column("finalAddress")

    def totalCollateral: Rep[CurrencyUnit] = column("totalCollateral")

    def refundSig: Rep[PartialSignature] = column("refundSig")

    def changeAddress: Rep[BitcoinAddress] = column("changeAddress")

    private type DLCTuple = (
        Sha256DigestBE,
        ECPublicKey,
        ECPublicKey,
        BitcoinAddress,
        CurrencyUnit,
        PartialSignature,
        BitcoinAddress)

    private val fromTuple: DLCTuple => DLCAcceptDb = {
      case (eventId,
            fundingKey,
            toLocalCETKey,
            finalAddress,
            totalCollateral,
            refundSig,
            changeAddress) =>
        DLCAcceptDb(eventId,
                    fundingKey,
                    toLocalCETKey,
                    finalAddress,
                    totalCollateral,
                    refundSig,
                    changeAddress)
    }

    private val toTuple: DLCAcceptDb => Option[DLCTuple] = dlc =>
      Some(
        (dlc.eventId,
         dlc.fundingKey,
         dlc.toLocalCETKey,
         dlc.finalAddress,
         dlc.totalCollateral,
         dlc.refundSig,
         dlc.changeAddress))

    def * : ProvenShape[DLCAcceptDb] =
      (eventId,
       fundingKey,
       toLocalCETKey,
       finalAddress,
       totalCollateral,
       refundSig,
       changeAddress) <> (fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc", sourceColumns = eventId)

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_eventId",
                 sourceColumns = eventId,
                 targetTableQuery = dlcTable)(_.eventId)
  }
}
