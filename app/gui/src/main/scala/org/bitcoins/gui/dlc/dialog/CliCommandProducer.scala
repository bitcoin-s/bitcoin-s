package org.bitcoins.gui.dlc.dialog

import org.bitcoins.commons.rpc.CliCommand

trait CliCommandProducer[T <: CliCommand] {
  def getCliCommand(): T
//  def buildView(params vary per dialog)
}
