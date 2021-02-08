package org.bitcoins.dlc.rewrite

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.DLCMessage.{DLCAccept, DLCOffer, DLCSign}
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.Sha256Digest
import scodec.bits.ByteVector

object DLCMessageBuilder {

  def buildContractInfo(
      totalCollateral: Satoshis,
      contractDescriptor: ContractDescriptor,
      oracleInfo: OracleInfo): ContractInfo = {
    ContractInfo(
      totalCollateral,
      ContractOraclePair.fromDescriptorOracle(contractDescriptor, oracleInfo))
  }

  def buildOffer(
      contractInfo: ContractInfo,
      publicKeys: DLCPublicKeys,
      offerCollateral: Satoshis,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts): DLCOffer = {
    DLCOffer(contractInfo,
             publicKeys,
             offerCollateral,
             fundingInputs,
             changeAddress,
             feeRate,
             timeouts)
  }

  def buildAccept(
      acceptCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      cetSigs: CETSignatures,
      negotiationFields: DLCAccept.NegotiationFields,
      tempContractId: Sha256Digest): DLCAccept = {
    DLCAccept(acceptCollateral,
              pubKeys,
              fundingInputs,
              changeAddress,
              cetSigs,
              negotiationFields,
              tempContractId)
  }

  def buildSign(
      cetSigs: CETSignatures, // TODO: compute in overload
      fundingSigs: FundingSignatures, // TODO: compute in overload
      contractId: ByteVector): DLCSign = {
    DLCSign(cetSigs, fundingSigs, contractId)
  }
}
