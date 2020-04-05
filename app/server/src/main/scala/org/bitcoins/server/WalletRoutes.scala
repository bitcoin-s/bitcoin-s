package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import org.bitcoins.appCommons.JsonSerializers
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.ptlc.PTLCMessage
import org.bitcoins.core.wallet.fee.{SatoshisPerByte, SatoshisPerVirtualByte}
import org.bitcoins.node.Node
import org.bitcoins.picklers._
import org.bitcoins.wallet.api.UnlockedWalletApi

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletRoutes(wallet: UnlockedWalletApi, node: Node)(
    implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher
  implicit val materializer = ActorMaterializer()

  /** Takes a string and turns into an escaped version of itself */
  private def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  private def handlePTLCMessage(
      ptlcMessage: PTLCMessage,
      escaped: Boolean): HttpEntity.Strict = {
    val json = JsonSerializers.toJson(ptlcMessage)
    val sendString =
      if (escaped) escape(json.toString()) else json.render(indent = 2)
    Server.httpSuccess(sendString)
  }

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

    // PLTCs
    case ServerCommand("createptlc", arr) =>
      CreatePTLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(CreatePTLC(amount, timeout, escaped)) =>
          complete {
            wallet.createPTLCInvoice(amount, timeout).map { invoice =>
              handlePTLCMessage(invoice, escaped)
            }
          }
      }

    case ServerCommand("acceptptlc", arr) =>
      AcceptPTLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(AcceptPTLC(invoice, feeRateOpt, escaped)) =>
          complete {
            val feeRate =
              feeRateOpt.getOrElse(SatoshisPerVirtualByte(Satoshis(3)))
            wallet.acceptPTLCInvoice(invoice, feeRate).map { accept =>
              handlePTLCMessage(accept, escaped)
            }
          }
      }

    case ServerCommand("signptlc", arr) =>
      SignPTLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SignPTLC(accept, escaped)) =>
          complete {
            wallet.signPTLC(accept).map { sig =>
              handlePTLCMessage(sig, escaped)
            }
          }
      }

    case ServerCommand("addptlcsig", arr) =>
      AddPTLCSig.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(AddPTLCSig(sig)) =>
          complete {
            wallet.addPTLCSig(sig).map { db =>
              Server.httpSuccess(s"Added signatures for PTLC ${db.invoiceId}")
            }
          }
      }

    case ServerCommand("broadcastptlc", arr) =>
      BroadcastPTLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(BroadcastPTLC(id)) =>
          complete {
            wallet.getPTLC(id).flatMap { tx =>
              node
                .broadcastTransaction(tx)
                .map(_ => Server.httpSuccess(tx.txIdBE.hex))
            }
          }
      }

    case ServerCommand("getptlc", arr) =>
      GetPTLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetPTLC(id)) =>
          complete {
            wallet.getPTLC(id).map { tx =>
              Server.httpSuccess(tx.hex)
            }
          }
      }

    case ServerCommand("claimptlc", arr) =>
      ClaimPTLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ClaimPTLC(id)) =>
          complete {
            wallet.claimPTLC(id).map { tx =>
              Server.httpSuccess(tx.hex)
            }
          }
      }

    case ServerCommand("getptlcsecret", arr) =>
      GetPTLCSecret.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetPTLCSecret(id, ptlcSpendTx)) =>
          complete {
            wallet.getPTLCSecret(id, ptlcSpendTx).map { key =>
              Server.httpSuccess(key)
            }
          }
      }

    case ServerCommand("refundptlc", arr) =>
      RefundPTLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(RefundPTLC(id)) =>
          complete {
            wallet.refundPTLC(id).map { tx =>
              Server.httpSuccess(tx.hex)
            }
          }
      }
  }
}
