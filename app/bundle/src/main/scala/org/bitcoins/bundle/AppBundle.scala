package org.bitcoins.bundle

import org.bitcoins.gui.WalletGUI
import org.bitcoins.server.{BitcoinSServer, BitcoinSServerMain}

import scala.concurrent.ExecutionContext

object AppBundle extends App {
  import ExecutionContext.Implicits.global
  BitcoinSServer.startedF.map(_ => WalletGUI.main(args))
  BitcoinSServerMain.main(args)
}
