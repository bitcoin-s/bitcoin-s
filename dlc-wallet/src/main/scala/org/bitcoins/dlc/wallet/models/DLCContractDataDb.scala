package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.BlockTimeStamp
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv.{ContractDescriptorTLV, OracleParamsV0TLV}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._

/** This table contains all the meta information about a DLC.
  * This includes various identifiers as well as state and a BIP 32 key path.
  */
case class DLCContractDataDb(
    dlcId: Sha256Digest,
    oracleThreshold: Int,
    oracleParamsTLVOpt: Option[OracleParamsV0TLV],
    contractDescriptorTLV: ContractDescriptorTLV,
    contractMaturity: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
    totalCollateral: CurrencyUnit,
    feeRate: SatoshisPerVirtualByte,
    fundOutputSerialId: UInt64
) {

  lazy val dlcTimeouts: DLCTimeouts =
    DLCTimeouts(contractMaturity, contractTimeout)
}
