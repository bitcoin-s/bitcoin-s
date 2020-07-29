package org.bitcoins.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.ContractInfo
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCOfferDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
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
    table.filter(_.eventId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[DLCOfferTable, DLCOfferDb, Seq] = {
    table
      .filter(_.eventId === id)
  }

  override def findAll(
      dlcs: Vector[DLCOfferDb]): Query[DLCOfferTable, DLCOfferDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.eventId))

  def findByEventId(eventId: Sha256DigestBE): Future[Option[DLCOfferDb]] = {
    val q = table.filter(_.eventId === eventId)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCOfferDb] =>
        throw new RuntimeException(
          s"More than one DLCOffer per eventId ($eventId), got: $dlcs")
    }
  }

  def findByEventId(eventId: Sha256Digest): Future[Option[DLCOfferDb]] =
    findByEventId(eventId.flip)

  class DLCOfferTable(tag: Tag)
      extends Table[DLCOfferDb](tag, "wallet_dlc_offers") {

    def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

    def network: Rep[BitcoinNetwork] = column("network")

    def oraclePubKey: Rep[SchnorrPublicKey] = column("oraclePubKey")

    def oracleRValue: Rep[SchnorrNonce] = column("oracleRValue")

    def contractInfo: Rep[ContractInfo] = column("contractInfo")

    def penaltyTimeout: Rep[UInt32] = column("penaltyTimeout")

    def contractMaturity: Rep[BlockTimeStamp] = column("contractMaturity")

    def contractTimeout: Rep[BlockTimeStamp] = column("contractTimeout")

    def fundingKey: Rep[ECPublicKey] = column("fundingKey")

    def toLocalCETKey: Rep[ECPublicKey] = column("toLocalCETKey")

    def finalAddress: Rep[BitcoinAddress] = column("finalAddress")

    def totalCollateral: Rep[CurrencyUnit] = column("totalCollateral")

    def feeRate: Rep[SatoshisPerVirtualByte] = column("feeRate")

    def changeAddress: Rep[BitcoinAddress] = column("changeAddress")

    private type DLCTuple = (
        Sha256DigestBE,
        BitcoinNetwork,
        SchnorrPublicKey,
        SchnorrNonce,
        ContractInfo,
        UInt32,
        BlockTimeStamp,
        BlockTimeStamp,
        ECPublicKey,
        ECPublicKey,
        BitcoinAddress,
        CurrencyUnit,
        SatoshisPerVirtualByte,
        BitcoinAddress)

    private val fromTuple: DLCTuple => DLCOfferDb = {
      case (eventId,
            network,
            oraclePubKey,
            oracleRValue,
            contractInfo,
            penaltyTimeout,
            contractMaturity,
            contractTimeout,
            fundingKey,
            toLocalCETKey,
            finalAddress,
            totalCollateral,
            feeRate,
            changeAddress) =>
        DLCOfferDb(
          eventId,
          network,
          oraclePubKey,
          oracleRValue,
          contractInfo,
          penaltyTimeout,
          contractMaturity,
          contractTimeout,
          fundingKey,
          toLocalCETKey,
          finalAddress,
          totalCollateral,
          feeRate,
          changeAddress
        )
    }

    private val toTuple: DLCOfferDb => Option[DLCTuple] = dlc =>
      Some(
        (dlc.eventId,
         dlc.network,
         dlc.oraclePubKey,
         dlc.oracleRValue,
         dlc.contractInfo,
         dlc.penaltyTimeout,
         dlc.contractMaturity,
         dlc.contractTimeout,
         dlc.fundingKey,
         dlc.toLocalCETKey,
         dlc.finalAddress,
         dlc.totalCollateral,
         dlc.feeRate,
         dlc.changeAddress))

    def * : ProvenShape[DLCOfferDb] =
      (eventId,
       network,
       oraclePubKey,
       oracleRValue,
       contractInfo,
       penaltyTimeout,
       contractMaturity,
       contractTimeout,
       fundingKey,
       toLocalCETKey,
       finalAddress,
       totalCollateral,
       feeRate,
       changeAddress) <> (fromTuple, toTuple)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_offer", sourceColumns = eventId)

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_eventId",
                 sourceColumns = eventId,
                 targetTableQuery = dlcTable)(_.eventId)
  }
}
