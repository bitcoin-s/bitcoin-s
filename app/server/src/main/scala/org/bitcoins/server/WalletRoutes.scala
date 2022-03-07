package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.stream.Materializer
import grizzled.slf4j.Logging
import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.api.dlc.wallet.AnyDLCHDWalletApi
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{AddressLabelTagType, TxoState}
import org.bitcoins.crypto.NetworkElement
import org.bitcoins.keymanager._
import org.bitcoins.keymanager.config.KeyManagerAppConfig
import org.bitcoins.server.routes.{Server, ServerCommand, ServerRoute}
import org.bitcoins.wallet.config.WalletAppConfig
import ujson._
import upickle.default._

import java.nio.file.{Files, Path}
import java.time.Instant
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class WalletRoutes(wallet: AnyDLCHDWalletApi)(implicit
    system: ActorSystem,
    walletConf: WalletAppConfig)
    extends ServerRoute
    with Logging {
  import system.dispatcher

  implicit val kmConf: KeyManagerAppConfig = walletConf.kmConf

  private def spendingInfoDbToJson(spendingInfoDb: SpendingInfoDb): Value = {
    Obj(
      "outpoint" -> Obj(
        "txid" -> Str(spendingInfoDb.txid.hex),
        "vout" -> Num(spendingInfoDb.outPoint.vout.toLong.toDouble)
      ),
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

  private def handleDestinationOpt(
      returnedStr: String,
      destinationOpt: Option[Path]
  ): String = {
    destinationOpt match {
      case Some(path) =>
        Files.write(path, returnedStr.getBytes)
        val absPath = path.toAbsolutePath.toString
        s"Written to $absPath"
      case None => returnedStr
    }
  }

  def handleCommand: PartialFunction[ServerCommand, Route] = {

    case ServerCommand("isempty", _) =>
      complete {
        wallet.isEmpty().map { empty =>
          Server.httpSuccess(empty)
        }
      }

    case ServerCommand("walletinfo", _) =>
      complete {
        getInfo.map { json =>
          Server.httpSuccess(json)
        }
      }

    case ServerCommand("getbalance", arr) =>
      withValidServerCommand(GetBalance.fromJsArr(arr)) {
        case GetBalance(isSats) =>
          complete {
            wallet.getBalance().map { balance =>
              val result: Double =
                formatCurrencyUnit(currencyUnit = balance, isSats = isSats)
              Server.httpSuccess(result)
            }
          }
      }

    case ServerCommand("getconfirmedbalance", arr) =>
      withValidServerCommand(GetBalance.fromJsArr(arr)) {
        case GetBalance(isSats) =>
          complete {
            wallet.getConfirmedBalance().map { balance =>
              val result: Double =
                formatCurrencyUnit(currencyUnit = balance, isSats = isSats)
              Server.httpSuccess(result)
            }
          }
      }

    case ServerCommand("getunconfirmedbalance", arr) =>
      withValidServerCommand(GetBalance.fromJsArr(arr)) {
        case GetBalance(isSats) =>
          complete {
            wallet.getUnconfirmedBalance().map { balance =>
              val result: Double =
                formatCurrencyUnit(currencyUnit = balance, isSats = isSats)
              Server.httpSuccess(result)
            }
          }
      }

    case ServerCommand("getbalances", arr) =>
      GetBalance.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(GetBalance(isSats)) =>
          complete {
            for {
              confirmed <- wallet.getConfirmedBalance()
              unconfirmed <- wallet.getUnconfirmedBalance()
              reservedUtxos <- wallet.listUtxos(TxoState.Reserved)
            } yield {

              val reserved = reservedUtxos.map(_.output.value).sum
              val total = confirmed + unconfirmed + reserved

              val json = Obj(
                "confirmed" -> Num(
                  formatCurrencyUnit(confirmed, isSats).toDouble),
                "unconfirmed" -> Num(
                  formatCurrencyUnit(unconfirmed, isSats).toDouble),
                "reserved" -> Num(
                  formatCurrencyUnit(reserved, isSats).toDouble),
                "total" -> Num(formatCurrencyUnit(total, isSats).toDouble)
              )
              Server.httpSuccess(json)
            }
          }
      }

    case ServerCommand("getnewaddress", arr) =>
      withValidServerCommand(GetNewAddress.fromJsArr(arr)) {
        case GetNewAddress(labelOpt) =>
          complete {
            val addressF = labelOpt match {
              case Some(label) =>
                wallet.getNewAddress(Vector(label))
              case None =>
                wallet.getNewAddress()
            }
            addressF.map(address => Server.httpSuccess(address))
          }
      }

    case ServerCommand("gettransaction", arr) =>
      withValidServerCommand(GetTransaction.fromJsArr(arr)) {
        case GetTransaction(txId) =>
          complete {
            wallet.findTransaction(txId).map {
              case None =>
                Server.httpSuccess(ujson.Null)
              case Some(txDb) =>
                Server.httpSuccess(txDb.transaction.hex)
            }
          }
      }

    case ServerCommand("lockunspent", arr) =>
      withValidServerCommand(LockUnspent.fromJsArr(arr)) {
        case LockUnspent(unlock, outputParams) =>
          complete {
            val func: Vector[SpendingInfoDb] => Future[Vector[SpendingInfoDb]] =
              utxos => {
                if (unlock) wallet.unmarkUTXOsAsReserved(utxos)
                else wallet.markUTXOsAsReserved(utxos)
              }

            for {
              utxos <-
                if (unlock) {
                  wallet.listUtxos(TxoState.Reserved)
                } else wallet.listUtxos()

              filtered =
                if (outputParams.nonEmpty) {
                  utxos.filter(utxo =>
                    outputParams.exists(_.outPoint == utxo.outPoint))
                } else utxos

              reserved <- func(filtered)
            } yield Server.httpSuccess(reserved.nonEmpty)
          }
      }

    case ServerCommand("labeladdress", arr) =>
      withValidServerCommand(LabelAddress.fromJsArr(arr)) {
        case LabelAddress(address, label) =>
          complete {
            wallet.tagAddress(address, label).map { tagDb =>
              Server.httpSuccess(
                s"Added label \'${tagDb.tagName.name}\' to ${tagDb.address.value}")
            }
          }
      }

    case ServerCommand("getaddresstags", arr) =>
      withValidServerCommand(GetAddressTags.fromJsArr(arr)) {
        case GetAddressTags(address) =>
          complete {
            wallet.getAddressTags(address).map { tagDbs =>
              val retStr = tagDbs.map(_.tagName.name)
              Server.httpSuccess(retStr)
            }
          }
      }

    case ServerCommand("getaddresslabels", arr) =>
      withValidServerCommand(GetAddressLabels.fromJsArr(arr)) {
        case GetAddressLabels(address) =>
          complete {
            wallet.getAddressTags(address, AddressLabelTagType).map { tagDbs =>
              val retStr = tagDbs.map(_.tagName.name)
              Server.httpSuccess(retStr)
            }
          }
      }

    case ServerCommand("dropaddresslabels", arr) =>
      withValidServerCommand(DropAddressLabels.fromJsArr(arr)) {
        case DropAddressLabels(address) =>
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

    case ServerCommand("getdlcoffer", arr) =>
      GetDLCOffer.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(GetDLCOffer(tempContractId)) =>
          complete {
            for {
              dlcOpt <- wallet.findDLCByTemporaryContractId(tempContractId)
              dlc = dlcOpt.getOrElse(
                throw new IllegalArgumentException(
                  s"Cannot find a DLC with temp contact ID $tempContractId"))
              offerOpt <- wallet.getDLCOffer(dlc.dlcId)
              offer = offerOpt.getOrElse(
                throw new IllegalArgumentException(
                  s"Cannot find an offer with for DLC ID ${dlc.dlcId}"))
            } yield {
              Server.httpSuccess(offer.toMessage.hex)
            }
          }
      }

    case ServerCommand("getdlcs", _) =>
      complete {
        wallet.listDLCs().map { dlcs =>
          Server.httpSuccess(dlcs.map(writeJs(_)))
        }
      }

    case ServerCommand("getdlc", arr) =>
      GetDLC.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(GetDLC(paramHash)) =>
          complete {
            wallet.findDLC(paramHash).map {
              case None => Server.httpSuccess(ujson.Null)
              case Some(dlc) =>
                Server.httpSuccess(writeJs(dlc))
            }
          }
      }

    case ServerCommand("canceldlc", arr) =>
      GetDLC.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(GetDLC(paramHash)) =>
          complete {
            wallet.cancelDLC(paramHash).map { _ =>
              Server.httpSuccess("Success")
            }
          }
      }

    case ServerCommand("createdlcoffer", arr) =>
      CreateDLCOffer.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(
              CreateDLCOffer(contractInfo,
                             collateral,
                             feeRateOpt,
                             locktime,
                             refundLT,
                             payoutAddressOpt,
                             changeAddressOpt)) =>
          complete {
            val announcements = contractInfo.oracleInfo match {
              case OracleInfoV0TLV(announcement)        => Vector(announcement)
              case OracleInfoV1TLV(_, announcements)    => announcements
              case OracleInfoV2TLV(_, announcements, _) => announcements
            }
            if (!announcements.forall(_.validateSignature)) {
              throw new RuntimeException(
                s"Received Oracle announcement with invalid signature! ${announcements
                  .map(_.hex)}")
            }

            wallet
              .createDLCOffer(contractInfo,
                              collateral,
                              feeRateOpt,
                              locktime,
                              refundLT,
                              payoutAddressOpt,
                              changeAddressOpt)
              .map { offer =>
                Server.httpSuccess(offer.toMessage.hex)
              }
          }
      }

    case ServerCommand("acceptdlcoffer", arr) =>
      AcceptDLCOffer.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(
              AcceptDLCOffer(offer, payoutAddressOpt, changeAddressOpt)) =>
          complete {
            wallet
              .acceptDLCOffer(offer.tlv, payoutAddressOpt, changeAddressOpt)
              .map { accept =>
                Server.httpSuccess(accept.toMessage.hex)
              }
          }
      }

    case ServerCommand("acceptdlcofferfromfile", arr) =>
      DLCDataFromFile.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(
              DLCDataFromFile(path,
                              destOpt,
                              payoutAddressOpt,
                              changeAddressOpt)) =>
          complete {

            val hex = Files.readAllLines(path).get(0)

            val offerMessage = LnMessageFactory(DLCOfferTLV).fromHex(hex)

            wallet
              .acceptDLCOffer(offerMessage.tlv,
                              payoutAddressOpt,
                              changeAddressOpt)
              .map { accept =>
                val ret = handleDestinationOpt(accept.toMessage.hex, destOpt)
                Server.httpSuccess(ret)
              }
          }
      }

    case ServerCommand("signdlc", arr) =>
      SignDLC.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
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
          complete(Server.httpBadRequest(exception))
        case Success(DLCDataFromFile(path, destOpt, _, _)) =>
          complete {

            val hex = Files.readAllLines(path).get(0)

            val acceptMessage = LnMessageFactory(DLCAcceptTLV).fromHex(hex)

            wallet
              .signDLC(acceptMessage.tlv)
              .map { sign =>
                val ret = handleDestinationOpt(sign.toMessage.hex, destOpt)
                Server.httpSuccess(ret)
              }
          }
      }

    case ServerCommand("adddlcsigs", arr) =>
      AddDLCSigs.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
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
          complete(Server.httpBadRequest(exception))
        case Success(DLCDataFromFile(path, _, _, _)) =>
          complete {

            val hex = Files.readAllLines(path).get(0)

            val signMessage = LnMessageFactory(DLCSignTLV).fromHex(hex)

            wallet.addDLCSigs(signMessage.tlv).map { db =>
              Server.httpSuccess(
                s"Successfully added sigs to DLC ${db.contractIdOpt.get.toHex}")
            }
          }
      }

    case ServerCommand("adddlcsigsandbroadcast", arr) =>
      AddDLCSigs.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(AddDLCSigs(sigs)) =>
          complete {
            for {
              _ <- wallet.addDLCSigs(sigs.tlv)
              tx <- wallet.broadcastDLCFundingTx(sigs.tlv.contractId)
            } yield Server.httpSuccess(tx.txIdBE.hex)
          }
      }

    case ServerCommand("adddlcsigsandbroadcastfromfile", arr) =>
      DLCDataFromFile.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(DLCDataFromFile(path, _, _, _)) =>
          val hex = Files.readAllLines(path).get(0)

          val signMessage = LnMessageFactory(DLCSignTLV).fromHex(hex)
          complete {
            for {
              _ <- wallet.addDLCSigs(signMessage.tlv)
              tx <- wallet.broadcastDLCFundingTx(signMessage.tlv.contractId)
            } yield Server.httpSuccess(tx.txIdBE.hex)
          }
      }

    case ServerCommand("getdlcfundingtx", arr) =>
      GetDLCFundingTx.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
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
          complete(Server.httpBadRequest(exception))
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
          complete(Server.httpBadRequest(exception))
        case Success(ExecuteDLC(contractId, sigs, noBroadcast)) =>
          complete {
            for {
              tx <- wallet.executeDLC(contractId, sigs)
              ret <- handleBroadcastable(tx, noBroadcast)
            } yield {
              Server.httpSuccess(ret.hex)
            }
          }
      }

    case ServerCommand("executedlcrefund", arr) =>
      ExecuteDLCRefund.fromJsArr(arr) match {
        case Failure(exception) =>
          complete(Server.httpBadRequest(exception))
        case Success(ExecuteDLCRefund(contractId, noBroadcast)) =>
          complete {
            for {
              tx <- wallet.executeDLCRefund(contractId)
              ret <- handleBroadcastable(tx, noBroadcast)
            } yield {
              Server.httpSuccess(ret.hex)
            }
          }
      }

    case ServerCommand("sendtoaddress", arr) =>
      withValidServerCommand(SendToAddress.fromJsArr(arr)) {
        case SendToAddress(address,
                           bitcoins,
                           satoshisPerVirtualByteOpt,
                           noBroadcast) =>
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
      withValidServerCommand(SendFromOutpoints.fromJsArr(arr)) {
        case SendFromOutpoints(outPoints,
                               address,
                               bitcoins,
                               satoshisPerVirtualByteOpt) =>
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

    case ServerCommand("sweepwallet", arr) =>
      withValidServerCommand(SweepWallet.fromJsArr(arr)) {
        case SweepWallet(address, feeRateOpt) =>
          complete {
            for {
              tx <- wallet.sweepWallet(address, feeRateOpt)
              _ <- wallet.broadcastTransaction(tx)
            } yield Server.httpSuccess(tx.txIdBE)
          }
      }

    case ServerCommand("sendwithalgo", arr) =>
      withValidServerCommand(SendWithAlgo.fromJsArr(arr)) {
        case SendWithAlgo(address, bitcoins, satoshisPerVirtualByteOpt, algo) =>
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

    case ServerCommand("signpsbt", arr) =>
      withValidServerCommand(SignPSBT.fromJsArr(arr)) { case SignPSBT(psbt) =>
        complete {
          wallet.signPSBT(psbt).map { signed =>
            Server.httpSuccess(signed.base64)
          }
        }
      }

    case ServerCommand("opreturncommit", arr) =>
      withValidServerCommand(OpReturnCommit.fromJsArr(arr)) {
        case OpReturnCommit(message, hashMessage, satoshisPerVirtualByteOpt) =>
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

    case ServerCommand("bumpfeerbf", arr) =>
      withValidServerCommand(BumpFee.fromJsArr(arr)) {
        case BumpFee(txId, feeRate) =>
          complete {
            for {
              tx <- wallet.bumpFeeRBF(txId, feeRate)
              _ <- wallet.broadcastTransaction(tx)
            } yield Server.httpSuccess(tx.txIdBE)
          }
      }

    case ServerCommand("bumpfeecpfp", arr) =>
      withValidServerCommand(BumpFee.fromJsArr(arr)) {
        case BumpFee(txId, feeRate) =>
          complete {
            for {
              tx <- wallet.bumpFeeCPFP(txId, feeRate)
              _ <- wallet.broadcastTransaction(tx)
            } yield Server.httpSuccess(tx.txIdBE)
          }
      }

    case ServerCommand("rescan", arr) =>
      withValidServerCommand(Rescan.fromJsArr(arr)) {
        case Rescan(batchSize,
                    startBlock,
                    endBlock,
                    force,
                    ignoreCreationTime) =>
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

    case ServerCommand("listreservedutxos", _) =>
      complete {
        wallet.listUtxos(TxoState.Reserved).map { utxos =>
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
          val addressAndValues = addressDbs.map { case (addressDb, value) =>
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
      withValidServerCommand(GetAddressInfo.fromJsArr(arr)) {
        case GetAddressInfo(address) =>
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

    case ServerCommand("keymanagerpassphrasechange", arr) =>
      withValidServerCommand(KeyManagerPassphraseChange.fromJsArr(arr)) {
        case KeyManagerPassphraseChange(oldPassword, newPassword) =>
          complete {
            val path = walletConf.seedPath
            WalletStorage.changeAesPassword(path,
                                            Some(oldPassword),
                                            Some(newPassword))

            Server.httpSuccess(ujson.Null)
          }
      }

    case ServerCommand("keymanagerpassphraseset", arr) =>
      withValidServerCommand(KeyManagerPassphraseSet.fromJsArr(arr)) {
        case KeyManagerPassphraseSet(password) =>
          complete {
            val path = walletConf.seedPath
            WalletStorage.changeAesPassword(path, None, Some(password))

            Server.httpSuccess(ujson.Null)
          }
      }

    case ServerCommand("importseed", arr) =>
      withValidServerCommand(ImportSeed.fromJsArr(arr)) {
        case ImportSeed(walletName, mnemonic, passwordOpt) =>
          complete {
            val seedPath = kmConf.seedFolder.resolve(
              s"$walletName-${WalletStorage.ENCRYPTED_SEED_FILE_NAME}")

            val creationTime = Instant.ofEpochSecond(WalletStorage.GENESIS_TIME)

            val mnemonicState = passwordOpt match {
              case Some(pass) =>
                DecryptedMnemonic(mnemonic, creationTime).encrypt(pass)
              case None =>
                DecryptedMnemonic(mnemonic, creationTime)
            }

            WalletStorage.writeSeedToDisk(seedPath, mnemonicState)

            Server.httpSuccess(ujson.Null)
          }
      }

    case ServerCommand("importxprv", arr) =>
      withValidServerCommand(ImportXprv.fromJsArr(arr)) {
        case ImportXprv(walletName, xprv, passwordOpt) =>
          complete {
            val seedPath = kmConf.seedFolder.resolve(
              s"$walletName-${WalletStorage.ENCRYPTED_SEED_FILE_NAME}")

            val creationTime = Instant.ofEpochSecond(WalletStorage.GENESIS_TIME)

            val mnemonicState = passwordOpt match {
              case Some(pass) =>
                DecryptedExtPrivKey(xprv, creationTime).encrypt(pass)
              case None =>
                DecryptedExtPrivKey(xprv, creationTime)
            }

            WalletStorage.writeSeedToDisk(seedPath, mnemonicState)

            Server.httpSuccess(ujson.Null)
          }
      }

    case ServerCommand("sendrawtransaction", arr) =>
      withValidServerCommand(SendRawTransaction.fromJsArr(arr)) {
        case SendRawTransaction(tx) =>
          complete {
            wallet.broadcastTransaction(tx).map { _ =>
              Server.httpSuccess(tx.txIdBE)
            }
          }
      }

    case ServerCommand("estimatefee", _) =>
      complete {
        val feeRateF = wallet
          .getFeeRate()
          .recover { case scala.util.control.NonFatal(exn) =>
            logger.error(
              s"Failed to fetch fee rate from wallet, returning -1 sats/vbyte",
              exn)
            SatoshisPerVirtualByte.negativeOne
          }

        feeRateF.map(f => Server.httpSuccess(f.toSatsPerVByte))
      }

    case ServerCommand("getdlcwalletaccounting", _) =>
      complete {
        wallet.getWalletAccounting().map { accounting =>
          Server.httpSuccess(writeJs(accounting))
        }
      }
  }

  /** Returns information about the state of our wallet */
  def getInfo: Future[Obj] = {
    for {
      accountDb <- wallet.getDefaultAccount()
      walletState <- wallet.getSyncState()
      rescan <- wallet.isRescanning()
    } yield {
      Obj(
        WalletAppConfig.moduleName ->
          Obj(
            KeyManagerAppConfig.moduleName -> Obj(
              "rootXpub" -> Str(wallet.keyManager.getRootXPub.toString)
            ),
            "walletName" -> Str(walletConf.walletNameOpt.getOrElse("")),
            "xpub" -> Str(accountDb.xpub.toString),
            "hdPath" -> Str(accountDb.hdAccount.toString),
            "height" -> Num(walletState.height),
            "blockHash" -> Str(walletState.blockHash.hex),
            "rescan" -> rescan
          )
      )
    }
  }

  private def formatCurrencyUnit(
      currencyUnit: CurrencyUnit,
      isSats: Boolean): Double = {
    if (isSats) {
      currencyUnit.satoshis.toBigDecimal.toDouble
    } else {
      Bitcoins(currencyUnit.satoshis).toBigDecimal.toDouble
    }
  }
}
