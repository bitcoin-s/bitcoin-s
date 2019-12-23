package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import org.bitcoins.core.currency._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.node.Node
import org.bitcoins.picklers._
import org.bitcoins.wallet.api.UnlockedWalletApi

import scala.util.{Failure, Success}

case class WalletRoutes(wallet: UnlockedWalletApi, node: Node)(
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

    case ServerCommand("rescan", arr) =>
      Rescan.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(Rescan(addresses, startBlock, endBlock)) =>
          complete {
            wallet
              .rescan(addresses.map(_.scriptPubKey), startBlock, endBlock)
              .map(_ => Server.httpSuccess("ok"))
          }
      }

  }
}
