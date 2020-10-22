package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.BroadcastDLCFundingTx
import org.bitcoins.crypto.Sha256Digest
import scalafx.scene.control.TextField

object GetFundingDLCDialog
    extends DLCDialog[BroadcastDLCFundingTx](
      "DLC Funding Transaction",
      "Enter DLC event ID",
      Vector(DLCDialog.dlcContractIdStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): BroadcastDLCFundingTx = {
    val eventId = Sha256Digest(inputs(dlcContractIdStr))
    BroadcastDLCFundingTx(eventId)
  }
}
