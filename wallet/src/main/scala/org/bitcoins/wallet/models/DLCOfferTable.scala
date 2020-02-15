package org.bitcoins.wallet.models

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage.{ContractInfo, DLCOffer, OracleInfo}
import org.bitcoins.dlc.{DLCPublicKeys, DLCTimeouts}
import slick.jdbc.SQLiteProfile.api._
import slick.lifted.{PrimaryKey, ProvenShape}

case class DLCOfferDb(
    eventId: Sha256DigestBE,
    network: BitcoinNetwork,
    oracleInfo: OracleInfo,
    contractInfo: ContractInfo,
    timeouts: DLCTimeouts,
    pubKeys: DLCPublicKeys,
    totalCollateral: CurrencyUnit,
    fundingInputs: Vector[OutputReference],
    feeRate: SatoshisPerVirtualByte,
    changeAddress: Bech32Address) {

  def toDLCOffer: DLCOffer = {
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

class DLCOfferTable(tag: Tag)
    extends Table[DLCOfferDb](tag, "wallet_dlc_offers") {

  import org.bitcoins.db.DbCommonsColumnMappers._

  def eventId: Rep[Sha256DigestBE] = column("eventId", O.Unique)

  def network: Rep[BitcoinNetwork] = column("network")

  def oracleInfo: Rep[OracleInfo] = column("oracleInfo")

  def contractInfo: Rep[ContractInfo] = column("contractInfo")

  def timeouts: Rep[DLCTimeouts] = column("timeouts")

  def pubKeys: Rep[DLCPublicKeys] = column("pubKeys")

  def totalCollateral: Rep[CurrencyUnit] = column("totalCollateral")

  def fundingInputs: Rep[Vector[OutputReference]] =
    column("fundingInputs")

  def feeRate: Rep[SatoshisPerVirtualByte] = column("feeRate")

  def changeAddress: Rep[Bech32Address] = column("changeAddress")

  private type DLCTuple = (
      Sha256DigestBE,
      BitcoinNetwork,
      OracleInfo,
      ContractInfo,
      DLCTimeouts,
      DLCPublicKeys,
      CurrencyUnit,
      Vector[OutputReference],
      SatoshisPerVirtualByte,
      Bech32Address)

  private val fromTuple: DLCTuple => DLCOfferDb = {
    case (eventId,
          network,
          oracleInfo,
          contractInfo,
          timeouts,
          pubKeys,
          totalCollateral,
          fundingInputs,
          feeRate,
          changeAddress) =>
      DLCOfferDb(
        eventId,
        network,
        oracleInfo,
        contractInfo,
        timeouts,
        pubKeys,
        totalCollateral,
        fundingInputs,
        feeRate,
        changeAddress
      )
  }

  private val toTuple: DLCOfferDb => Option[DLCTuple] = dlc =>
    Some(
      (dlc.eventId,
       dlc.network,
       dlc.oracleInfo,
       dlc.contractInfo,
       dlc.timeouts,
       dlc.pubKeys,
       dlc.totalCollateral,
       dlc.fundingInputs,
       dlc.feeRate,
       dlc.changeAddress))

  def * : ProvenShape[DLCOfferDb] =
    (eventId,
     network,
     oracleInfo,
     contractInfo,
     timeouts,
     pubKeys,
     totalCollateral,
     fundingInputs,
     feeRate,
     changeAddress) <> (fromTuple, toTuple)

  def primaryKey: PrimaryKey =
    primaryKey(name = "pk_dlc", sourceColumns = eventId)
}
