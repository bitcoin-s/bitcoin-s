package org.bitcoins.core.dlc.template

import org.bitcoins.core.protocol.dlc.models.ContractInfo
import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV

abstract class DLCTemplate {

  def createContractInfo(
      oracleAnnouncementTLV: OracleAnnouncementTLV,
      parameters: Map[String, String]): ContractInfo
}
