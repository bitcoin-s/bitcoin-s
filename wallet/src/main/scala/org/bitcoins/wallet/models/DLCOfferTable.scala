package org.bitcoins.wallet.models

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStampWithFuture}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCOffer,
  OracleInfo
}
import org.bitcoins.commons.jsonmodels.dlc.{
  DLCMessage,
  DLCPublicKeys,
  DLCTimeouts
}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

case class DLCOfferDb(
    eventId: Sha256DigestBE,
    network: BitcoinNetwork,
    oraclePubKey: SchnorrPublicKey,
    oracleRValue: SchnorrNonce,
    contractInfo: ContractInfo,
    penaltyTimeout: UInt32,
    contractMaturity: BlockStampWithFuture,
    contractTimeout: BlockStampWithFuture,
    fundingKey: ECPublicKey,
    toLocalCETKey: ECPublicKey,
    finalAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    feeRate: SatoshisPerVirtualByte,
    changeAddress: BitcoinAddress) {

  def toDLCOffer(fundingInputs: Vector[OutputReference]): DLCOffer = {
    val oracleInfo = OracleInfo(oraclePubKey, oracleRValue)
    val pubKeys =
      DLCPublicKeys(fundingKey, toLocalCETKey, finalAddress)
    val timeouts =
      DLCTimeouts(penaltyTimeout, contractMaturity, contractTimeout)
    DLCOffer(
      contractInfo,
      oracleInfo,
      pubKeys,
      totalCollateral.satoshis,
      fundingInputs,
      changeAddress,
      feeRate,
      timeouts
    )
  }
}

object DLCOfferDb {

  def fromDLCOffer(offer: DLCOffer, network: BitcoinNetwork): DLCOfferDb = {
    val eventId = DLCMessage.calcEventId(offer.oracleInfo,
                                         offer.contractInfo,
                                         offer.timeouts)
    DLCOfferDb(
      eventId,
      network,
      offer.oracleInfo.pubKey,
      offer.oracleInfo.rValue,
      offer.contractInfo,
      offer.timeouts.penaltyTimeout,
      offer.timeouts.contractMaturity,
      offer.timeouts.contractTimeout,
      offer.pubKeys.fundingKey,
      offer.pubKeys.toLocalCETKey,
      offer.pubKeys.finalAddress,
      offer.totalCollateral,
      offer.feeRate,
      offer.changeAddress
    )
  }
}

class DLCOfferTable(tag: Tag)
    extends Table[DLCOfferDb](tag, "wallet_dlc_offers") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

  def network: Rep[BitcoinNetwork] = column("network")

  def oraclePubKey: Rep[SchnorrPublicKey] = column("oraclePubKey")

  def oracleRValue: Rep[SchnorrNonce] = column("oracleRValue")

  def contractInfo: Rep[ContractInfo] = column("contractInfo")

  def penaltyTimeout: Rep[UInt32] = column("penaltyTimeout")

  def contractMaturity: Rep[BlockStampWithFuture] = column("contractMaturity")

  def contractTimeout: Rep[BlockStampWithFuture] = column("contractTimeout")

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
      BlockStampWithFuture,
      BlockStampWithFuture,
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
    primaryKey(name = "pk_dlc", sourceColumns = eventId)

  def fk: ForeignKeyQuery[DLCTable, DLCDb] =
    foreignKey("fk_eventId",
               sourceColumns = eventId,
               targetTableQuery = TableQuery[DLCTable])(_.eventId)
}
