package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc.{
  DLCFundingInput,
  DLCPublicKeys,
  DLCTimeouts
}
import org.bitcoins.core.protocol.tlv.ContractInfoV0TLV
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._

case class DLCOfferDb(
    paramHash: Sha256DigestBE,
    tempContractId: Sha256Digest,
    contractInfoTLV: ContractInfoV0TLV,
    contractMaturity: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
    fundingKey: ECPublicKey,
    payoutAddress: BitcoinAddress,
    totalCollateral: CurrencyUnit,
    feeRate: SatoshisPerVirtualByte,
    changeAddress: BitcoinAddress) {

  lazy val contractInfo: ContractInfo = ContractInfo.fromTLV(contractInfoTLV)

  lazy val oracleInfo: OracleInfo = contractInfo.oracleInfo

  lazy val dlcPubKeys: DLCPublicKeys = DLCPublicKeys(fundingKey, payoutAddress)

  lazy val dlcTimeouts: DLCTimeouts =
    DLCTimeouts(contractMaturity, contractTimeout)

  def toDLCOffer(fundingInputs: Vector[DLCFundingInput]): DLCOffer = {

    DLCOffer(
      contractInfo,
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
      offer.contractInfo.toTLV,
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
