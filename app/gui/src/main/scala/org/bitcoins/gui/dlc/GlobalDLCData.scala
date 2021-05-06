package org.bitcoins.gui.dlc

import org.bitcoins.core.protocol.dlc.DLCStatus
import scalafx.collections.ObservableBuffer

object GlobalDLCData {
  var lastContractId: String = ""
  var lastOracleSig: String = ""
  var lastOracleAnnouncement: String = ""
  var lastContractInfo: String = ""

  val dlcs: ObservableBuffer[DLCStatus] =
    new ObservableBuffer[DLCStatus]()
}
