package org.bitcoins.gui.ptlc.dialog

import org.bitcoins.cli.CliCommand.GetPTLCSecret
import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.protocol.transaction.Transaction
import scalafx.scene.control.TextField

object GetPTLCSecretDialog
    extends PTLCDialog[GetPTLCSecret](
      "Get PTLC Secret",
      "Enter PTLC Info",
      Vector(PTLCDialog.invoiceIdStr -> new TextField(),
             PTLCDialog.spendTxStr -> PTLCDialog.textArea())) {
  import PTLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): GetPTLCSecret = {
    val invoiceId = Sha256DigestBE(inputs(invoiceIdStr))
    val spendTx = Transaction.fromHex(inputs(spendTxStr))
    GetPTLCSecret(invoiceId, spendTx)
  }
}
