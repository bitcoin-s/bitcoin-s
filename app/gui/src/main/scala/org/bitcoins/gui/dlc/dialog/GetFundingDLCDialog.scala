package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.GetDLCFundingTx
import org.bitcoins.crypto.Sha256Digest
import scalafx.scene.control.TextField

object GetFundingDLCDialog
    extends DLCDialog[GetDLCFundingTx](
      "DLC Funding Transaction",
      "Enter DLC event ID",
      Vector(DLCDialog.dlcContractIdStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): GetDLCFundingTx = {
    val eventId = Sha256Digest(inputs(dlcContractIdStr))
    GetDLCFundingTx(eventId)
  }
}
