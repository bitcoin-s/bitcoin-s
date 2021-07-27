package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand

trait CliCommandProducer {
  def getCliCommand(): Option[CliCommand]
//  def buildView(params vary per dialog)
}
