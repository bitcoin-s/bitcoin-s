package org.bitcoins.wallet.models

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
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._

case class DLCOfferDb(
    eventId: Sha256DigestBE,
    oraclePubKey: SchnorrPublicKey,
    oracleRValue: SchnorrNonce,
    contractInfo: ContractInfo,
    contractMaturity: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
    fundingKey: ECPublicKey,
    payoutAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    feeRate: SatoshisPerVirtualByte,
    changeAddress: BitcoinAddress) {

  def toDLCOffer(fundingInputs: Vector[OutputReference]): DLCOffer = {
    val oracleInfo = OracleInfo(oraclePubKey, oracleRValue)
    val pubKeys = DLCPublicKeys(fundingKey, payoutAddress)
    val timeouts = DLCTimeouts(contractMaturity, contractTimeout)
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

object DLCOfferDbHelper {

  def fromDLCOffer(offer: DLCOffer): DLCOfferDb = {
    val eventId = DLCMessage.calcEventId(offer.oracleInfo,
                                         offer.contractInfo,
                                         offer.timeouts)
    DLCOfferDb(
      eventId,
      offer.oracleInfo.pubKey,
      offer.oracleInfo.rValue,
      offer.contractInfo,
      offer.timeouts.contractMaturity,
      offer.timeouts.contractTimeout,
      offer.pubKeys.fundingKey,
      offer.pubKeys.payoutAddress,
      offer.totalCollateral,
      offer.feeRate,
      offer.changeAddress
    )
  }
}
