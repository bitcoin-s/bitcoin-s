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
    val sendString =
      if (escaped) escape(str) else ujson.read(str).render(indent = 2)
    Server.httpSuccess(sendString)
  }

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

    case ServerCommand("createdlcoffer", arr) =>
      CreateDLCOffer.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
            CreateDLCOffer(oracleInfo,
                           contractInfo,
                           collateral,
                           feeRateOpt,
                           locktime,
                           refundLT,
                           escaped)) =>
          complete {
            wallet
              .createDLCOffer(oracleInfo,
                              contractInfo,
                              collateral,
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

    case ServerCommand("initdlcmutualclose", arr) =>
      InitDLCMutualClose.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(InitDLCMutualClose(eventId, oracleSig, escaped)) =>
          complete {
            wallet.initDLCMutualClose(eventId, oracleSig).map {
              handleDLCMessage(_, escaped)
            }
          }
      }

    case ServerCommand("acceptdlcmutualclose", arr) =>
      AcceptDLCMutualClose.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(AcceptDLCMutualClose(mutualCloseSig)) =>
          complete {
            wallet.acceptDLCMutualClose(mutualCloseSig).map { tx =>
              Server.httpSuccess(tx.hex)
            }
          }
      }

    case ServerCommand("getdlcfundingtx", arr) =>
      GetDLCFundingTx.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetDLCFundingTx(eventId)) =>
          complete {
            wallet.getDLCFundingTx(eventId).map { tx =>
              Server.httpSuccess(tx.hex)
            }
          }
      }

    case ServerCommand("executedlcunilateralclose", arr) =>
      ExecuteDLCUnilateralClose.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ExecuteDLCUnilateralClose(eventId, oracleSig)) =>
          complete {
            wallet.executeDLCUnilateralClose(eventId, oracleSig).map { txs =>
              txs._2 match {
                case Some(closingTx) =>
                  Server.httpSuccess(s"${txs._1.hex} \n ${closingTx.hex}")
                case None =>
                  Server.httpSuccess(txs._1.hex)
              }
            }
          }
      }

    case ServerCommand("executedlcremoteunilateralclose", arr) =>
      ExecuteDLCRemoteUnilateralClose.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ExecuteDLCRemoteUnilateralClose(eventId, cet)) =>
          complete {
            wallet.executeRemoteUnilateralDLC(eventId, cet).map {
              case Some(closingTx) =>
                Server.httpSuccess(closingTx.hex)
              case None =>
                Server.httpSuccess(
                  "Recieved would have only been dust, they have been used as fees")
            }
          }
      }

    case ServerCommand("executedlcforceclose", arr) =>
      ExecuteDLCForceClose.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ExecuteDLCForceClose(eventId, oracleSig)) =>
          complete {
            wallet.executeDLCForceClose(eventId, oracleSig).map { txs =>
              txs._2 match {
                case Some(closingTx) =>
                  Server.httpSuccess(s"${txs._1.hex} \n ${closingTx.hex}")
                case None =>
                  Server.httpSuccess(txs._1.hex)
              }
            }
          }
      }

    case ServerCommand("claimdlcremotefunds", arr) =>
      ClaimDLCRemoteFunds.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ClaimDLCRemoteFunds(eventId, tx)) =>
          complete {
            wallet.claimDLCRemoteFunds(eventId, tx).map {
              case Some(closingTx) =>
                Server.httpSuccess(closingTx.hex)
              case None =>
                Server.httpSuccess(
                  "Recieved would have only been dust, they have been used as fees")
            }
          }
      }

    case ServerCommand("executedlcrefund", arr) =>
      ExecuteDLCRefund.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ExecuteDLCRefund(eventId)) =>
          complete {
            wallet.executeDLCRefund(eventId).map { txs =>
              txs._2 match {
                case Some(closingTx) =>
                  Server.httpSuccess(s"${txs._1.hex} \n ${closingTx.hex}")
                case None =>
                  Server.httpSuccess(txs._1.hex)
              }
            }
          }
      }

    case ServerCommand("claimdlcpenaltyfunds", arr) =>
      ClaimDLCPenaltyFunds.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ClaimDLCPenaltyFunds(eventId, tx)) =>
          complete {
            wallet.claimDLCPenaltyFunds(eventId, tx).map {
              case Some(closingTx) =>
                Server.httpSuccess(closingTx.hex)
              case None =>
                Server.httpSuccess(
                  "Recieved would have only been dust, they have been used as fees")
            }
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

    case ServerCommand("decoderawtransaction", arr) =>
      DecodeRawTransaction.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(DecodeRawTransaction(tx)) =>
          complete {
            val jsonStr = wallet.decodeRawTransaction(tx)
            Server.httpSuccess(jsonStr)
          }
      }
  }
}
