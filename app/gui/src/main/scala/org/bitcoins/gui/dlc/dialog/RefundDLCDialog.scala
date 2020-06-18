package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.ExecuteDLCRefund
import org.bitcoins.crypto.Sha256DigestBE
import scalafx.scene.control.TextField

object RefundDLCDialog
    extends DLCDialog[ExecuteDLCRefund](
      "DLC Refund",
      "Enter DLC event ID",
      Vector(DLCDialog.dlcEventIdStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): ExecuteDLCRefund = {
    val eventId = Sha256DigestBE(inputs(dlcEventIdStr))
    ExecuteDLCRefund(eventId, noBroadcast = false)
  }
}
