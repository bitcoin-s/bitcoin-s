package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.ExecuteDLCRefund
import scalafx.scene.Node
import scalafx.scene.control.TextField
import scodec.bits.ByteVector

class RefundDLCDialog
    extends DLCDialog[ExecuteDLCRefund](
      "DLC Refund",
      "Enter DLC contract ID",
      Vector(DLCDialog.dlcContractIdStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, Node]): ExecuteDLCRefund = {
    val hex = readStringFromNode(inputs(dlcContractIdStr))

    val contractId = ByteVector.fromValidHex(hex)
    ExecuteDLCRefund(contractId, noBroadcast = false)
  }
}
