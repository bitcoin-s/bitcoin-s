package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.CreatePTLC
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import scalafx.scene.control.TextField

object CreatePTLCDialog
    extends PTLCDialog[CreatePTLC](
      "Create PTLC Invoice",
      "Enter PTLC Information",
      Vector(PTLCDialog.amountStr -> new TextField(),
             PTLCDialog.timeoutStr -> new TextField())) {
  import PTLCDialog._

  override def constructFromInput(inputs: Map[String, String]): CreatePTLC = {
    val amount = Satoshis(BigInt(inputs(amountStr)))
    val timeout = UInt32(inputs(timeoutStr).toLong)
    CreatePTLC(amount, timeout, escaped = false)
  }
}
