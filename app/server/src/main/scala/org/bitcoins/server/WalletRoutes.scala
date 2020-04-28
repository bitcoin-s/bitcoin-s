package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import org.bitcoins.core.currency._
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.node.Node
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.wallet.api.WalletApi

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletRoutes(wallet: WalletApi, node: Node)(
    implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {

    case ServerCommand("isempty", _) =>
      complete {
        wallet.isEmpty().map { empty =>
          Server.httpSuccess(empty)
        }
      }

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

    case ServerCommand("getconfirmedbalance", arr) =>
      GetBalance.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetBalance(isSats)) =>
          complete {
            wallet.getConfirmedBalance().map { balance =>
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

    case ServerCommand("getunconfirmedbalance", arr) =>
      GetBalance.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetBalance(isSats)) =>
          complete {
            wallet.getUnconfirmedBalance().map { balance =>
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

    case ServerCommand("getutxos", _) =>
      complete {
        wallet.listUtxos().map { utxos =>
          val retStr = utxos.foldLeft("")((accum, spendInfo) =>
            accum + s"${spendInfo.outPoint.hex} ${spendInfo.output.value}\n")
          Server.httpSuccess(retStr)
        }
      }

    case ServerCommand("getaddresses", _) =>
      complete {
        wallet.listAddresses().map { addressDbs =>
          val addresses = addressDbs.map(_.address)
          Server.httpSuccess(addresses)
        }
      }

    case ServerCommand("getaccounts", _) =>
      complete {
        wallet.listAccounts().map { accounts =>
          val xpubs = accounts.map(_.xpub)
          Server.httpSuccess(xpubs)
        }
      }

    case ServerCommand("createnewaccount", _) =>
      complete {
        for {
          newWallet <- wallet.createNewAccount(wallet.keyManager.kmParams)
          accounts <- newWallet.listAccounts()
        } yield {
          val xpubs = accounts.map(_.xpub)
          Server.httpSuccess(xpubs)
        }
      }

  }
}
