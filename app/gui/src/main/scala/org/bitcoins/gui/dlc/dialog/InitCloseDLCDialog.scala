package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.InitDLCMutualClose
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}
import scalafx.scene.control.TextField

object InitCloseDLCDialog
    extends DLCDialog[InitDLCMutualClose](
      "DLC Funding Transaction",
      "Enter DLC closing info",
      Vector(DLCDialog.dlcEventIdStr -> new TextField(),
             DLCDialog.dlcOracleSigStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): InitDLCMutualClose = {
    val eventId = Sha256DigestBE(inputs(dlcEventIdStr))
    val oracleSig = SchnorrDigitalSignature(inputs(dlcOracleSigStr))
    InitDLCMutualClose(eventId, oracleSig, escaped = false)
  }
}
