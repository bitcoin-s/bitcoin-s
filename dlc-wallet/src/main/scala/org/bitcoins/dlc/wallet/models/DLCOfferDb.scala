package org.bitcoins.dlc.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCOffer,
  OracleInfo
}
import org.bitcoins.commons.jsonmodels.dlc.{
  DLCFundingInput,
  DLCPublicKeys,
  DLCTimeouts
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._

case class DLCOfferDb(
    paramHash: Sha256DigestBE,
    tempContractId: Sha256Digest,
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

  lazy val oracleInfo: OracleInfo = OracleInfo(oraclePubKey, oracleRValue)

  lazy val dlcPubKeys: DLCPublicKeys = DLCPublicKeys(fundingKey, payoutAddress)

  lazy val dlcTimeouts: DLCTimeouts =
    DLCTimeouts(contractMaturity, contractTimeout)

  def toDLCOffer(fundingInputs: Vector[DLCFundingInput]): DLCOffer = {

    DLCOffer(
      contractInfo,
      oracleInfo,
      dlcPubKeys,
      totalCollateral.satoshis,
      fundingInputs,
      changeAddress,
      feeRate,
      dlcTimeouts
    )
  }
}

object DLCOfferDbHelper {

  def fromDLCOffer(offer: DLCOffer): DLCOfferDb = {
    DLCOfferDb(
      offer.paramHash,
      offer.tempContractId,
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
