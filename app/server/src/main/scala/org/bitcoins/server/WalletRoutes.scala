package org.bitcoins.server

import java.nio.file.Files

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.Materializer
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.utxo.AddressLabelTagType
import org.bitcoins.crypto.NetworkElement
import org.bitcoins.dlc.wallet.AnyDLCHDWalletApi
import ujson._

import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletRoutes(wallet: AnyDLCHDWalletApi)(implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher

  private def spendingInfoDbToJson(spendingInfoDb: SpendingInfoDb): Value = {
    Obj(
      "outpoint" -> Str(spendingInfoDb.outPoint.hex),
      "value" -> Num(spendingInfoDb.output.value.satoshis.toLong.toDouble)
    )
  }

  implicit val materializer: Materializer =
    Materializer.createMaterializer(system)

  private def handleBroadcastable(
      tx: Transaction,
      noBroadcast: Boolean): Future[NetworkElement] = {
    if (noBroadcast) {
      Future.successful(tx)
    } else {
      wallet.broadcastTransaction(tx).map(_ => tx.txIdBE)
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

    case ServerCommand("getnewaddress", arr) =>
      GetNewAddress.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetNewAddress(labelOpt)) =>
          complete {
            val labelVec = Vector(labelOpt).flatten
            wallet.getNewAddress(labelVec).map { address =>
              Server.httpSuccess(address)
            }
          }
      }

    case ServerCommand("lockunspent", arr) =>
      LockUnspent.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(LockUnspent(unlock, outputParams)) =>
          complete {
            val func: Vector[SpendingInfoDb] => Future[Vector[SpendingInfoDb]] =
              utxos => {
                if (unlock) wallet.unmarkUTXOsAsReserved(utxos)
                else wallet.markUTXOsAsReserved(utxos)
              }

            for {
              utxos <- {
                if (outputParams.nonEmpty) {
                  wallet
                    .listUtxos()
                    .map(_.filter(utxo =>
                      outputParams.exists(_.outPoint == utxo.outPoint)))
                } else {
                  wallet.listUtxos()
                }
              }
              reserved <- func(utxos)
            } yield {
              if (reserved.nonEmpty) {
                Server.httpSuccess(true)
              } else {
                Server.httpSuccess(false)
              }
            }
          }
      }

    case ServerCommand("labeladdress", arr) =>
      LabelAddress.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(LabelAddress(address, label)) =>
          complete {
            wallet.tagAddress(address, label).map { tagDb =>
              Server.httpSuccess(
                s"Added label \'${tagDb.tagName.name}\' to ${tagDb.address.value}")
            }
          }
      }

    case ServerCommand("getaddresstags", arr) =>
      GetAddressTags.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetAddressTags(address)) =>
          complete {
            wallet.getAddressTags(address).map { tagDbs =>
              val retStr = tagDbs.map(_.tagName.name)
              Server.httpSuccess(retStr)
            }
          }
      }

    case ServerCommand("getaddresslabels", arr) =>
      GetAddressLabels.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetAddressLabels(address)) =>
          complete {
            wallet.getAddressTags(address, AddressLabelTagType).map { tagDbs =>
              val retStr = tagDbs.map(_.tagName.name)
              Server.httpSuccess(retStr)
            }
          }
      }

    case ServerCommand("dropaddresslabels", arr) =>
      DropAddressLabels.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(DropAddressLabels(address)) =>
          complete {
            wallet.dropAddressTagType(address, AddressLabelTagType).map {
              numDropped =>
                if (numDropped <= 0) {
                  Server.httpSuccess(s"Address had no labels")
                } else if (numDropped == 1) {
                  Server.httpSuccess(s"$numDropped label dropped")
                } else {
                  Server.httpSuccess(s"$numDropped labels dropped")
                }
            }
          }
      }

    case ServerCommand("getdlcs", _) =>
      complete {
        wallet.listDLCs().map { dlcs =>
          Server.httpSuccess(dlcs.map(_.toJson))
        }
      }

    case ServerCommand("getdlc", arr) =>
      GetDLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetDLC(paramHash)) =>
          complete {
            wallet.findSerializedDLC(paramHash).map {
              case None => Server.httpSuccess(ujson.Null)
              case Some(dlc) =>
                Server.httpSuccess(dlc.toJson)
            }
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
                             refundLT)) =>
          complete {
            wallet
              .createDLCOffer(oracleInfo,
                              contractInfo,
                              collateral,
                              feeRateOpt,
                              locktime,
                              refundLT)
              .map { offer =>
                Server.httpSuccess(offer.toMessage.hex)
              }
          }
      }

    case ServerCommand("acceptdlcoffer", arr) =>
      AcceptDLCOffer.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(AcceptDLCOffer(offer)) =>
          complete {
            wallet
              .acceptDLCOffer(offer.tlv)
              .map { accept =>
                Server.httpSuccess(accept.toMessage.hex)
              }
          }
      }

    case ServerCommand("acceptdlcofferfromfile", arr) =>
      DLCDataFromFile.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(DLCDataFromFile(path)) =>
          complete {

            val hex = Files.readAllLines(path).get(0)

            val offerMessage = LnMessageFactory(DLCOfferTLV).fromHex(hex)

            wallet
              .acceptDLCOffer(offerMessage.tlv)
              .map { accept =>
                Server.httpSuccess(accept.toMessage.hex)
              }
          }
      }

    case ServerCommand("signdlc", arr) =>
      SignDLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SignDLC(accept)) =>
          complete {
            wallet
              .signDLC(accept.tlv)
              .map { sign =>
                Server.httpSuccess(sign.toMessage.hex)
              }
          }
      }

    case ServerCommand("signdlcfromfile", arr) =>
      DLCDataFromFile.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(DLCDataFromFile(path)) =>
          complete {

            val hex = Files.readAllLines(path).get(0)

            val acceptMessage = LnMessageFactory(DLCAcceptTLV).fromHex(hex)

            wallet
              .signDLC(acceptMessage.tlv)
              .map { sign =>
                Server.httpSuccess(sign.toMessage.hex)
              }
          }
      }

    case ServerCommand("adddlcsigs", arr) =>
      AddDLCSigs.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(AddDLCSigs(sigs)) =>
          complete {
            wallet.addDLCSigs(sigs.tlv).map { db =>
              Server.httpSuccess(
                s"Successfully added sigs to DLC ${db.contractIdOpt.get.toHex}")
            }
          }
      }

    case ServerCommand("adddlcsigsfromfile", arr) =>
      DLCDataFromFile.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(DLCDataFromFile(path)) =>
          complete {

            val hex = Files.readAllLines(path).get(0)

            val signMessage = LnMessageFactory(DLCSignTLV).fromHex(hex)

            wallet.addDLCSigs(signMessage.tlv).map { db =>
              Server.httpSuccess(
                s"Successfully added sigs to DLC ${db.contractIdOpt.get.toHex}")
            }
          }
      }

    case ServerCommand("getdlcfundingtx", arr) =>
      GetDLCFundingTx.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetDLCFundingTx(contractId)) =>
          complete {
            wallet.getDLCFundingTx(contractId).map { tx =>
              Server.httpSuccess(tx.hex)
            }
          }
      }

    case ServerCommand("broadcastdlcfundingtx", arr) =>
      BroadcastDLCFundingTx.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(BroadcastDLCFundingTx(contractId)) =>
          complete {
            wallet.broadcastDLCFundingTx(contractId).map { tx =>
              Server.httpSuccess(tx.txIdBE.hex)
            }
          }
      }

    case ServerCommand("executedlc", arr) =>
      ExecuteDLC.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ExecuteDLC(contractId, oracleSigs, noBroadcast)) =>
          complete {
            for {
              tx <- wallet.executeDLC(contractId, oracleSigs)
              retStr <- handleBroadcastable(tx, noBroadcast)
            } yield {
              Server.httpSuccess(retStr.hex)
            }
          }
      }

    case ServerCommand("executedlcrefund", arr) =>
      ExecuteDLCRefund.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(ExecuteDLCRefund(contractId, noBroadcast)) =>
          complete {
            for {
              tx <- wallet.executeDLCRefund(contractId)
              retStr <- handleBroadcastable(tx, noBroadcast)
            } yield {
              Server.httpSuccess(retStr.hex)
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
                        batchSize.getOrElse(wallet.discoveryBatchSize()),
                      useCreationTime = !ignoreCreationTime)
                  Future.successful("Rescan started.")
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
          val json = utxos.map(spendingInfoDbToJson)
          Server.httpSuccess(json)
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
            case (addressDb, value) =>
              Obj(
                "address" -> Str(addressDb.address.value),
                "value" -> Num(value.satoshis.toLong.toDouble)
              )
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

    case ServerCommand("getaddressinfo", arr) =>
      GetAddressInfo.fromJsArr(arr) match {
        case Failure(err) =>
          reject(ValidationRejection("failure", Some(err)))
        case Success(GetAddressInfo(address)) =>
          complete {
            wallet.getAddressInfo(address).map {
              case Some(addressInfo) =>
                val json = Obj(
                  "pubkey" -> Str(addressInfo.pubkey.hex),
                  "path" -> Str(addressInfo.path.toString)
                )
                Server.httpSuccess(json)
              case None =>
                Server.httpSuccess("Wallet does not contain address")
            }
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
