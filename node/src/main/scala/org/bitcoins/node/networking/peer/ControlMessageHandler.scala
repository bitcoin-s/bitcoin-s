package org.bitcoins.node.networking.peer

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.db.DbConfig
import org.bitcoins.node.messages._
import org.bitcoins.node.messages.control.PongMessage
import org.bitcoins.node.networking.Client

import scala.concurrent.ExecutionContext

class ControlMessageHandler(dbConfig: DbConfig)(implicit ec: ExecutionContext) extends BitcoinSLogger {


  def handleControlPayload(controlMsg: ControlPayload, client: Client) : Unit = {
    controlMsg match {
      case pingMsg: PingMessage =>

        client.peer ! PongMessage(pingMsg.nonce)

      case SendHeadersMessage =>

        //not implemented as of now
        ()
      case _: AddrMessage =>

        ()
      case _@(_: FilterAddMessage | _: FilterLoadMessage |
                      FilterClearMessage) =>
        ()
      case _@(GetAddrMessage | VerAckMessage | _: VersionMessage |
                       _: PongMessage) =>
        ()
      case _: RejectMessage =>
        ()

      case _: FeeFilterMessage =>
        ()
    }
  }
}
