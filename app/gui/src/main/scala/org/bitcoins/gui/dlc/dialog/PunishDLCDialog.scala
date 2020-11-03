package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.ClaimDLCPenaltyFunds
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.Sha256DigestBE
import scalafx.scene.control.TextField

object PunishDLCDialog
    extends DLCDialog[ClaimDLCPenaltyFunds](
      "DLC Force Close",
      "Enter DLC punishment info",
      Vector(DLCDialog.dlcEventIdStr -> new TextField(),
             DLCDialog.dlcForceCloseTxStr -> DLCDialog.textArea())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): ClaimDLCPenaltyFunds = {
    val eventId = Sha256DigestBE(inputs(dlcEventIdStr))
    val forceCloseTx = Transaction(inputs(dlcForceCloseTxStr))
    ClaimDLCPenaltyFunds(eventId, forceCloseTx, noBroadcast = false)
  }
}
