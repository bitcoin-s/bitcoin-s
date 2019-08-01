package org.bitcoins.server

import org.bitcoins.picklers._

import akka.actor.ActorSystem
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import org.bitcoins.core.currency._
import org.bitcoins.wallet.api.UnlockedWalletApi
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.node.SpvNode

import de.heikoseeberger.akkahttpupickle.UpickleSupport._
import scala.util.Failure
import scala.util.Success

case class WalletRoutes(wallet: UnlockedWalletApi, node: SpvNode)(
    implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getbalance", _) =>
      complete {
        wallet.getBalance().map { balance =>
          Server.httpSuccess(
            Bitcoins(balance.satoshis)
          )
        }
      }
    case ServerCommand("getnewaddress", _) =>
      complete {
        wallet.getNewAddress().map { address =>
          Server.httpSuccess(address)
        }
      }

    case ServerCommand("sendtoaddress", arr) =>
      // TODO create custom directive for this?
      SendToAddress.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SendToAddress(address, bitcoins)) =>
          complete {
            // TODO dynamic fees
            val feeRate = SatoshisPerByte(100.sats)
            wallet.sendToAddress(address, bitcoins, feeRate).map { tx =>
              node.broadcastTransaction(tx)
              Server.httpSuccess(tx.txIdBE)
            }
          }
      }

  }
}
