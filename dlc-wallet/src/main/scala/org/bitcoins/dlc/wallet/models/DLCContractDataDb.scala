package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BlockTimeStamp
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv.{ContractDescriptorTLV, OracleParamsV0TLV}
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
    totalCollateral: CurrencyUnit
) {

  lazy val dlcTimeouts: DLCTimeouts =
    DLCTimeouts(contractMaturity, contractTimeout)
}
