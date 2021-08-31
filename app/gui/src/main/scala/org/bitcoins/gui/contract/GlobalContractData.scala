package org.bitcoins.gui.contract

import org.bitcoins.core.protocol.tlv.{
  ContractInfoV0TLV,
  OracleAnnouncementV0TLV
}
import scalafx.collections.ObservableBuffer

object GlobalContractData {

  val announcements: ObservableBuffer[
    (OracleAnnouncementV0TLV, Option[ContractInfoV0TLV])] =
    new ObservableBuffer[(OracleAnnouncementV0TLV, Option[ContractInfoV0TLV])]()
}
