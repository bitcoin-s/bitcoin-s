package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.tlv._
import scalafx.scene.Node

class SignDLCDialog
    extends DLCDialog[SignDLCCliCommand]("Sign DLC",
                                         "Enter DLC Accept message",
                                         Vector(
                                           DLCDialog.dlcAcceptStr -> DLCDialog
                                             .textArea(),
                                           "Open Accept from File" ->
                                             DLCDialog.fileChooserButton { file =>
                                               DLCDialog.acceptDLCFile =
                                                 Some(file)
                                               DLCDialog.acceptFileChosenLabel.text =
                                                 file.toString
                                             },
                                           DLCDialog.fileChosenStr -> DLCDialog.acceptFileChosenLabel
                                         ),
                                         Vector(DLCDialog.dlcAcceptStr,
                                                DLCDialog.dlcAcceptFileStr)) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, Node]): SignDLCCliCommand = {
    acceptDLCFile match {
      case Some(file) =>
        acceptDLCFile = None // reset
        acceptFileChosenLabel.text = "" // reset
        SignDLCFromFile(file.toPath)
      case None =>
        val acceptHex = readStringFromNode(inputs(dlcAcceptStr))

        val accept = LnMessageFactory(DLCAcceptTLV).fromHex(acceptHex)
        SignDLC(accept)
    }
  }
}
