package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd._
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BlockTimeStamp
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv.{ContractDescriptorTLV, OracleParamsV0TLV}
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import scodec.bits.ByteVector

case class DLCDb(
    dlcId: Sha256Digest,
    tempContractId: Sha256Digest,
    contractIdOpt: Option[ByteVector],
    protocolVersion: Int,
    state: DLCState,
    isInitiator: Boolean,
    account: HDAccount,
    changeIndex: HDChainType,
    keyIndex: Int,
    oracleThreshold: Int,
    oracleParamsTLVOpt: Option[OracleParamsV0TLV],
    contractDescriptorTLV: ContractDescriptorTLV,
    contractMaturity: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
    totalCollateral: CurrencyUnit,
    feeRate: SatoshisPerVirtualByte,
    fundOutputSerialId: UInt64,
    fundingOutPointOpt: Option[TransactionOutPoint],
    fundingTxIdOpt: Option[DoubleSha256DigestBE],
    closingTxIdOpt: Option[DoubleSha256DigestBE],
    aggregateSignatureOpt: Option[SchnorrDigitalSignature]
) {

  lazy val dlcTimeouts: DLCTimeouts =
    DLCTimeouts(contractMaturity, contractTimeout)

  def updateState(newState: DLCState): DLCDb = {
    copy(state = newState)
  }

  def updateFundingOutPoint(outPoint: TransactionOutPoint): DLCDb = {
    copy(fundingOutPointOpt = Some(outPoint),
         fundingTxIdOpt = Some(outPoint.txIdBE))
  }
}
