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
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.OutputReference
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._

case class DLCOfferDb(
    eventId: Sha256DigestBE,
    network: BitcoinNetwork,
    oraclePubKey: SchnorrPublicKey,
    oracleRValue: SchnorrNonce,
    contractInfo: ContractInfo,
    penaltyTimeout: UInt32,
    contractMaturity: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
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
