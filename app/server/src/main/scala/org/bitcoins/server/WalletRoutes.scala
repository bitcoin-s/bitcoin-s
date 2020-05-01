package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.Materializer
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.NetworkElement
import org.bitcoins.node.Node
import org.bitcoins.wallet.WalletLogger
import org.bitcoins.wallet.api.WalletApi

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletRoutes(wallet: WalletApi, node: Node)(implicit
    system: ActorSystem)
    extends ServerRoute
    with WalletLogger {
  import system.dispatcher

  implicit val materializer: Materializer =
    Materializer.createMaterializer(system)

  /** Takes a string and turns into an escaped version of itself */
  private def escape(raw: String): String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(raw)).toString
  }

  private def handleDLCMessage(
      dlcMessage: DLCMessage,
      escaped: Boolean): HttpEntity.Strict = {
    val json = dlcMessage.toJson
    val sendString =
      if (escaped) escape(json.toString()) else json.render(indent = 2)
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
        case Success(GetDLCFundingTx(eventId)) =>
          complete {
            wallet.getDLCFundingTx(eventId).map { tx =>
              Server.httpSuccess(tx.hex)
            }
          }
      }

    case ServerCommand("broadcastdlcfundingtx", arr) =>
      BroadcastDLCFundingTx.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(BroadcastDLCFundingTx(eventId)) =>
          complete {
            wallet.getDLCFundingTx(eventId).flatMap { tx =>
              node.broadcastTransaction(tx).map { _ =>
                Server.httpSuccess(tx.txIdBE.hex)
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
                      closingRetStr <-
                        handleBroadcastable(closingTx, noBroadcast)
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
            for {
              tx <- wallet.sendToAddress(address,
                                         bitcoins,
                                         satoshisPerVirtualByteOpt)
              retStr <- handleBroadcastable(tx, noBroadcast)
            } yield {
              Server.httpSuccess(retStr.hex)
            }
          }
      }

    case ServerCommand("sendfromoutpoints", arr) =>
      SendFromOutpoints.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
              SendFromOutpoints(outPoints,
                                address,
                                bitcoins,
                                satoshisPerVirtualByteOpt)) =>
          complete {
            for {
              tx <- wallet.sendFromOutPoints(outPoints,
                                             address,
                                             bitcoins,
                                             satoshisPerVirtualByteOpt)
              _ <- wallet.broadcastTransaction(tx)
            } yield Server.httpSuccess(tx.txIdBE)
          }
      }

    case ServerCommand("sendwithalgo", arr) =>
      SendWithAlgo.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
              SendWithAlgo(address,
                           bitcoins,
                           satoshisPerVirtualByteOpt,
                           algo)) =>
          complete {
            for {
              tx <- wallet.sendWithAlgo(address,
                                        bitcoins,
                                        satoshisPerVirtualByteOpt,
                                        algo)
              _ <- wallet.broadcastTransaction(tx)
            } yield Server.httpSuccess(tx.txIdBE)
          }
      }

    case ServerCommand("opreturncommit", arr) =>
      OpReturnCommit.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
              OpReturnCommit(message,
                             hashMessage,
                             satoshisPerVirtualByteOpt)) =>
          complete {
            for {
              tx <- wallet.makeOpReturnCommitment(message,
                                                  hashMessage,
                                                  satoshisPerVirtualByteOpt)
              _ <- wallet.broadcastTransaction(tx)
            } yield {
              Server.httpSuccess(tx.txIdBE)
            }
          }
      }

    case ServerCommand("rescan", arr) =>
      Rescan.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
              Rescan(batchSize,
                     startBlock,
                     endBlock,
                     force,
                     ignoreCreationTime)) =>
          complete {
            val res = for {
              empty <- wallet.isEmpty()
              msg <-
                if (force || empty) {
                  wallet
                    .rescanNeutrinoWallet(
                      startOpt = startBlock,
                      endOpt = endBlock,
                      addressBatchSize =
                        batchSize.getOrElse(wallet.discoveryBatchSize),
                      useCreationTime = !ignoreCreationTime)
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

    case ServerCommand("getspentaddresses", _) =>
      complete {
        wallet.listSpentAddresses().map { addressDbs =>
          val addresses = addressDbs.map(_.address)
          Server.httpSuccess(addresses)
        }
      }

    case ServerCommand("getfundedaddresses", _) =>
      complete {
        wallet.listFundedAddresses().map { addressDbs =>
          val addressAndValues = addressDbs.map {
            case (addressDb, value) => s"${addressDb.address} $value"
          }

          Server.httpSuccess(addressAndValues)
        }
      }

    case ServerCommand("getunusedaddresses", _) =>
      complete {
        wallet.listUnusedAddresses().map { addressDbs =>
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
