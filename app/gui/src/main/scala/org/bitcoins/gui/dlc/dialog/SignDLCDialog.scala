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
                                             DLCDialog.fileChooserButton(
                                               open = true,
                                               { file =>
                                                 DLCDialog.acceptDLCFile =
                                                   Some(file)
                                                 DLCDialog.acceptFileChosenLabel.text =
                                                   file.toString
                                               }),
                                           DLCDialog.fileChosenStr -> DLCDialog.acceptFileChosenLabel,
                                           DLCDialog.dlcSignFileDestStr ->
                                             DLCDialog.fileChooserButton(
                                               open = false,
                                               { file =>
                                                 DLCDialog.signDestDLCFile =
                                                   Some(file)
                                                 DLCDialog.signDestFileChosenLabel.text =
                                                   file.toString
                                               }),
                                           DLCDialog.fileChosenStr -> DLCDialog.signDestFileChosenLabel
                                         ),
                                         Vector(DLCDialog.dlcAcceptStr,
                                                DLCDialog.dlcAcceptFileStr,
                                                DLCDialog.dlcSignFileDestStr)) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, Node]): SignDLCCliCommand = {
    acceptDLCFile match {
      case Some(file) =>
        acceptDLCFile = None // reset
        acceptFileChosenLabel.text = "" // reset
        val destPathOpt = signDestDLCFile
        signDestDLCFile = None // reset
        signFileChosenLabel.text = "" // reset

        SignDLCFromFile(file.toPath, destPathOpt.map(_.toPath))
      case None =>
        val acceptHex = readStringFromNode(inputs(dlcAcceptStr))

        val accept = LnMessageFactory(DLCAcceptTLV).fromHex(acceptHex)
        SignDLC(accept)
    }
  }
}
