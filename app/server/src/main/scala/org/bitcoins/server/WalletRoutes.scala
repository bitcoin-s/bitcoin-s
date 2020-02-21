package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import org.bitcoins.core.currency._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.dlc.DLCMessage
import org.bitcoins.node.Node
import org.bitcoins.picklers._
import org.bitcoins.wallet.WalletLogger
import org.bitcoins.wallet.api.UnlockedWalletApi

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletRoutes(wallet: UnlockedWalletApi, node: Node)(
    implicit system: ActorSystem)
    extends ServerRoute
    with WalletLogger {
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  /** Takes a string and turns into an escaped version of itself */
  private def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  private def handleDLCMessage(
      dlcMessage: DLCMessage,
      escaped: Boolean): HttpEntity.Strict = {
    val str = dlcMessage.toJsonStr
    val sendString = if (escaped) escape(str) else str
    Server.httpSuccess(sendString)
  }

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getbalance", arr) =>
      GetBalance.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetBalance(isSats)) =>
          complete {
            wallet.getBalance().map { balance =>
              Server.httpSuccess(
                if (isSats) {
                  balance.satoshis.toString
                } else {
                  Bitcoins(balance.satoshis).toString
                }
              )
            }
          }
      }

    case ServerCommand("getnewaddress", _) =>
      complete {
        wallet.getNewAddress().map { address =>
          Server.httpSuccess(address)
        }
      }

    case ServerCommand("createdlcoffer", arr) =>
      CreateDLCOffer.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
            CreateDLCOffer(oracleInfo,
                           contractInfo,
                           feeRateOpt,
                           locktime,
                           refundLT,
                           escaped)) =>
          complete {
            wallet
              .createDLCOffer(oracleInfo,
                              contractInfo,
                              feeRateOpt,
                              locktime,
                              refundLT)
              .map(handleDLCMessage(_, escaped))
          }
      }

    case ServerCommand("acceptdlcoffer", arr) =>
      AcceptDLCOffer.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(AcceptDLCOffer(offer, escaped)) =>
          complete {
            wallet
              .acceptDLCOffer(offer)
              .map(handleDLCMessage(_, escaped))
          }
      }

    case ServerCommand("signdlc", arr) =>
      SignDLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SignDLC(accept, escaped)) =>
          complete {
            wallet
              .signDLC(accept)
              .map(handleDLCMessage(_, escaped))
          }
      }

    case ServerCommand("adddlcsigs", arr) =>
      AddDLCSigs.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(AddDLCSigs(sigs)) =>
          complete {
            wallet.addDLCSigs(sigs).map { _ =>
              Server.httpSuccess(
                s"Successfully added sigs to DLC ${sigs.eventId.hex}")
            }
          }
      }

    case ServerCommand("sendtoaddress", arr) =>
      // TODO create custom directive for this?
      SendToAddress.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
            SendToAddress(address, bitcoins, satoshisPerVirtualByteOpt)) =>
          complete {
            // TODO dynamic fees based off mempool and recent blocks
            val feeRate =
              satoshisPerVirtualByteOpt.getOrElse(SatoshisPerByte(100.satoshis))
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
        case Success(Rescan(batchSize, startBlock, endBlock, force)) =>
          complete {
            val res = for {
              empty <- wallet.isEmpty()
              msg <- if (force || empty) {
                wallet
                  .rescanNeutrinoWallet(
                    startBlock,
                    endBlock,
                    batchSize.getOrElse(wallet.discoveryBatchSize))
                  .map(_ => "scheduled")
              } else {
                Future.successful(
                  "DANGER! The wallet is not empty, however the rescan " +
                    "process destroys all existing records and creates new ones. " +
                    "Use force option if you really want to proceed. " +
                    "Don't forget to backup the wallet database.")
              }
            } yield msg
            res.map(msg => Server.httpSuccess(msg))
          }
      }

  }
}
