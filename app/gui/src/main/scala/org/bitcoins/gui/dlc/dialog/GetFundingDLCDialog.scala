package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.BroadcastDLCFundingTx
import org.bitcoins.crypto.Sha256DigestBE
import scalafx.scene.control.TextField

object GetFundingDLCDialog
    extends DLCDialog[BroadcastDLCFundingTx](
      "DLC Funding Transaction",
      "Enter DLC event ID",
      Vector(DLCDialog.dlcEventIdStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): BroadcastDLCFundingTx = {
    val eventId = Sha256DigestBE(inputs(dlcEventIdStr))
    BroadcastDLCFundingTx(eventId)
  }
}
