package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.BroadcastDLCFundingTx
import scalafx.scene.Node
import scalafx.scene.control.TextField
import scodec.bits.ByteVector

class GetFundingDLCDialog
    extends DLCDialog[BroadcastDLCFundingTx](
      "DLC Funding Transaction",
      "Enter DLC contract ID",
      Vector(DLCDialog.dlcContractIdStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, Node]): BroadcastDLCFundingTx = {
    val hex = readStringFromNode(inputs(dlcContractIdStr))

    val contractId = ByteVector.fromValidHex(hex)
    BroadcastDLCFundingTx(contractId)
  }
}
