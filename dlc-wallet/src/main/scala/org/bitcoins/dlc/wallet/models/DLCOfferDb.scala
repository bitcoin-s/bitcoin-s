package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCMessage._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._

case class DLCOfferDb(
    dlcId: Sha256Digest,
    fundingKey: ECPublicKey,
    payoutAddress: BitcoinAddress,
    payoutSerialId: UInt64,
    collateral: CurrencyUnit,
    changeAddress: BitcoinAddress,
    changeSerialId: UInt64) {

  val dlcPubKeys: DLCPublicKeys = DLCPublicKeys(fundingKey, payoutAddress)

  def toDLCOffer(
      contractInfo: ContractInfo,
      fundingInputs: Vector[DLCFundingInput],
      dlcDb: DLCDb,
      contractDataDb: DLCContractDataDb): DLCOffer = {
    toDLCOffer(contractInfo,
               fundingInputs,
               dlcDb.fundOutputSerialId,
               dlcDb.feeRate,
               contractDataDb.dlcTimeouts)
  }

  def toDLCOffer(
      contractInfo: ContractInfo,
      fundingInputs: Vector[DLCFundingInput],
      fundOutputSerialId: UInt64,
      feeRate: SatoshisPerVirtualByte,
      dlcTimeouts: DLCTimeouts): DLCOffer = {
    DLCOffer(
      contractInfo = contractInfo,
      pubKeys = dlcPubKeys,
      totalCollateral = collateral.satoshis,
      fundingInputs = fundingInputs,
      changeAddress = changeAddress,
      payoutSerialId = payoutSerialId,
      changeSerialId = changeSerialId,
      fundOutputSerialId = fundOutputSerialId,
      feeRate = feeRate,
      timeouts = dlcTimeouts
    )
  }
}

object DLCOfferDbHelper {

  def fromDLCOffer(id: Sha256Digest, offer: DLCOffer): DLCOfferDb = {
    DLCOfferDb(
      id,
      offer.pubKeys.fundingKey,
      offer.pubKeys.payoutAddress,
      offer.payoutSerialId,
      offer.totalCollateral,
      offer.changeAddress,
      offer.changeSerialId
    )
  }
}
