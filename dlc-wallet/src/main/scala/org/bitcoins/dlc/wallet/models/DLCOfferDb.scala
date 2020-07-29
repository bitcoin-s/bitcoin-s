package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCOffer
import org.bitcoins.core.protocol.dlc.models._
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
    payoutSerialId: UInt64,
    totalCollateral: CurrencyUnit,
    feeRate: SatoshisPerVirtualByte,
    changeAddress: BitcoinAddress,
    changeSerialId: UInt64,
    fundOutputSerialId: UInt64) {

  lazy val contractInfo: ContractInfo = ContractInfo.fromTLV(contractInfoTLV)

  lazy val oracleInfo: OracleInfo = contractInfo.oracleInfo

  lazy val dlcPubKeys: DLCPublicKeys = DLCPublicKeys(fundingKey, payoutAddress)

  lazy val dlcTimeouts: DLCTimeouts =
    DLCTimeouts(contractMaturity, contractTimeout)

  def toDLCOffer(fundingInputs: Vector[DLCFundingInput]): DLCOffer = {
    DLCOffer(
      contractInfo = contractInfo,
      pubKeys = dlcPubKeys,
      totalCollateral = totalCollateral.satoshis,
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

  def fromDLCOffer(offer: DLCOffer): DLCOfferDb = {
    DLCOfferDb(
      offer.paramHash,
      offer.tempContractId,
      offer.contractInfo.toTLV,
      offer.timeouts.contractMaturity,
      offer.timeouts.contractTimeout,
      offer.pubKeys.fundingKey,
      offer.pubKeys.payoutAddress,
      offer.payoutSerialId,
      offer.totalCollateral,
      offer.feeRate,
      offer.changeAddress,
      offer.changeSerialId,
      offer.fundOutputSerialId
    )
  }
}
