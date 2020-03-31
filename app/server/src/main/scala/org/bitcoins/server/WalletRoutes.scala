package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.ActorMaterializer
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.protocol.transaction.Transaction
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
  implicit val materializer: ActorMaterializer = ActorMaterializer()

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

  private def handleBroadcastable(
      tx: Transaction,
      noBroadcast: Boolean): Future[NetworkElement] = {
    if (noBroadcast) {
      Future.successful(tx)
    } else {
      node.broadcastTransaction(tx).map(_ => tx.txIdBE)
    }
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
        case Success(AcceptDLCMutualClose(mutualCloseSig, noBroadcast)) =>
          complete {
            wallet.acceptDLCMutualClose(mutualCloseSig).flatMap { tx =>
              handleBroadcastable(tx, noBroadcast).map { retStr =>
                Server.httpSuccess(retStr.hex)
              }
            }
          }
      }

    case ServerCommand("getdlcfundingtx", arr) =>
      GetDLCFundingTx.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetDLCFundingTx(eventId, noBroadcast)) =>
          complete {
            wallet.getDLCFundingTx(eventId).flatMap { tx =>
              handleBroadcastable(tx, noBroadcast).map { retStr =>
                Server.httpSuccess(retStr.hex)
              }
            }
          }
      }

    case ServerCommand("executedlcunilateralclose", arr) =>
      ExecuteDLCUnilateralClose.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
            ExecuteDLCUnilateralClose(eventId, oracleSig, noBroadcast)) =>
          complete {
            wallet.executeDLCUnilateralClose(eventId, oracleSig).flatMap {
              txs =>
                txs._2 match {
                  case Some(closingTx) =>
                    for {
                      retStr <- handleBroadcastable(txs._1, noBroadcast)
                      closingRetStr <- handleBroadcastable(closingTx,
                                                           noBroadcast)
                    } yield Server.httpSuccess(s"$retStr\n$closingRetStr")
                  case None =>
                    handleBroadcastable(txs._1, noBroadcast).map { retStr =>
                      Server.httpSuccess(retStr.hex)
                    }
                }
            }
          }
      }

    case ServerCommand("executedlcremoteunilateralclose", arr) =>
      ExecuteDLCRemoteUnilateralClose.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
            ExecuteDLCRemoteUnilateralClose(eventId, cet, noBroadcast)) =>
          complete {
            wallet.executeRemoteUnilateralDLC(eventId, cet).flatMap {
              case Some(closingTx) =>
                handleBroadcastable(closingTx, noBroadcast).map { retStr =>
                  Server.httpSuccess(retStr.hex)
                }
              case None =>
                Future.successful(
                  Server.httpSuccess(
                    "Received would have only been dust, they have been used as fees")
                )
            }
          }
      }

    case ServerCommand("executedlcforceclose", arr) =>
      ExecuteDLCForceClose.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ExecuteDLCForceClose(eventId, oracleSig, noBroadcast)) =>
          complete {
            wallet.executeDLCForceClose(eventId, oracleSig).map { txs =>
              txs._2 match {
                case Some(closingTx) =>
                  for {
                    retStr <- handleBroadcastable(txs._1, noBroadcast)
                    closingRetStr <- handleBroadcastable(closingTx, noBroadcast)
                  } yield Server.httpSuccess(s"$retStr\n$closingRetStr")
                case None =>
                  handleBroadcastable(txs._1, noBroadcast).map { retStr =>
                    Server.httpSuccess(retStr.hex)
                  }
              }
            }
          }
      }

    case ServerCommand("claimdlcremotefunds", arr) =>
      ClaimDLCRemoteFunds.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ClaimDLCRemoteFunds(eventId, tx, noBroadcast)) =>
          complete {
            wallet.claimDLCRemoteFunds(eventId, tx).flatMap {
              case Some(closingTx) =>
                handleBroadcastable(closingTx, noBroadcast).map { retStr =>
                  Server.httpSuccess(retStr.hex)
                }
              case None =>
                Future.successful(
                  Server.httpSuccess(
                    "Received would have only been dust, they have been used as fees")
                )
            }
          }
      }

    case ServerCommand("executedlcrefund", arr) =>
      ExecuteDLCRefund.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ExecuteDLCRefund(eventId, noBroadcast)) =>
          complete {
            wallet.executeDLCRefund(eventId).map { txs =>
              txs._2 match {
                case Some(closingTx) =>
                  for {
                    retStr <- handleBroadcastable(txs._1, noBroadcast)
                    closingRetStr <- handleBroadcastable(closingTx, noBroadcast)
                  } yield Server.httpSuccess(s"$retStr\n$closingRetStr")
                case None =>
                  handleBroadcastable(txs._1, noBroadcast).map { retStr =>
                    Server.httpSuccess(retStr.hex)
                  }
              }
            }
          }
      }

    case ServerCommand("claimdlcpenaltyfunds", arr) =>
      ClaimDLCPenaltyFunds.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ClaimDLCPenaltyFunds(eventId, tx, noBroadcast)) =>
          complete {
            wallet.claimDLCPenaltyFunds(eventId, tx).flatMap {
              case Some(closingTx) =>
                handleBroadcastable(closingTx, noBroadcast).map { retStr =>
                  Server.httpSuccess(retStr.hex)
                }
              case None =>
                Future.successful(
                  Server.httpSuccess(
                    "Received would have only been dust, they have been used as fees")
                )
            }
          }
      }

    case ServerCommand("sendtoaddress", arr) =>
      // TODO create custom directive for this?
      SendToAddress.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
            SendToAddress(address,
                          bitcoins,
                          satoshisPerVirtualByteOpt,
                          noBroadcast)) =>
          complete {
            // TODO dynamic fees based off mempool and recent blocks
            val feeRate =
              satoshisPerVirtualByteOpt.getOrElse(SatoshisPerByte(100.satoshis))
            wallet.sendToAddress(address, bitcoins, feeRate).map { tx =>
              handleBroadcastable(tx, noBroadcast).map { retStr =>
                Server.httpSuccess(retStr.hex)
              }
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
