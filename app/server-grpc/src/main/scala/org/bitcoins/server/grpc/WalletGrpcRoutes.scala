package org.bitcoins.server.grpc

import io.grpc.Status
import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.wallet.CoinSelectionAlgo
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  AddressLabelTagName,
  AddressLabelTagType,
  TxoState
}
import org.bitcoins.commons.util.WalletNames
import org.bitcoins.core.crypto.{ExtPrivateKey, MnemonicCode}
import org.bitcoins.crypto.{AesPassword, DoubleSha256DigestBE}
import org.bitcoins.keymanager.{
  DecryptedExtPrivKey,
  DecryptedMnemonic,
  WalletStorage
}
import org.bitcoins.keymanager.config.KeyManagerAppConfig

import java.nio.file.{Files, Path}
import java.time.Instant
import scala.util.{Failure, Success, Try}

import scala.concurrent.{ExecutionContext, Future}

/** gRPC service implementation for wallet endpoints.
  *
  * This implements a subset of [[org.bitcoins.server.WalletRoutes]] over gRPC.
  */
class WalletGrpcRoutes(
    wallet: DLCNeutrinoHDWalletApi,
    lifecycle: WalletGrpcLifecycleApi)(implicit ec: ExecutionContext)
    extends WalletRoutes {

  private def formatCurrencyUnit(
      currencyUnit: CurrencyUnit,
      isSats: Boolean): Double = {
    if (isSats) {
      currencyUnit.satoshis.toBigDecimal.toDouble
    } else {
      Bitcoins(currencyUnit.satoshis).toBigDecimal.toDouble
    }
  }

  private def toUtxo(utxo: SpendingInfoDb): Utxo = {
    Utxo(
      txid = utxo.txid.hex,
      vout = utxo.outPoint.vout.toLong,
      valueSats = utxo.output.value.satoshis.toLong
    )
  }

  private def toSatoshisPerVbOpt(
      feeRateOpt: Option[Long]): Option[SatoshisPerVirtualByte] =
    feeRateOpt.map(r => SatoshisPerVirtualByte(Satoshis(r)))

  private def parseBlockStampOpt(str: String): Option[BlockStamp] =
    if (str.isEmpty) None else Some(BlockStamp.fromString(str))

  private def handleTagDropped(numDropped: Int): String = {
    if (numDropped <= 0) "Address had no labels"
    else if (numDropped == 1) s"$numDropped label dropped"
    else s"$numDropped labels dropped"
  }

  // ── Existing endpoints ─────────────────────────────────────────────────────

  override def isEmpty(in: IsEmptyRequest): Future[IsEmptyResponse] = {
    wallet.isEmpty().map(empty => IsEmptyResponse(empty = empty))
  }

  override def getBalance(in: GetBalanceRequest): Future[GetBalanceResponse] = {
    wallet
      .getBalance()
      .map(balance =>
        GetBalanceResponse(
          balance = formatCurrencyUnit(balance, isSats = in.isSats)))
  }

  override def getConfirmedBalance(
      in: GetBalanceRequest): Future[GetBalanceResponse] = {
    wallet
      .getConfirmedBalance()
      .map(balance =>
        GetBalanceResponse(
          balance = formatCurrencyUnit(balance, isSats = in.isSats)))
  }

  override def getUnconfirmedBalance(
      in: GetBalanceRequest): Future[GetBalanceResponse] = {
    wallet
      .getUnconfirmedBalance()
      .map(balance =>
        GetBalanceResponse(
          balance = formatCurrencyUnit(balance, isSats = in.isSats)))
  }

  override def getBalances(
      in: GetBalancesRequest): Future[GetBalancesResponse] = {
    for {
      confirmed <- wallet.getConfirmedBalance()
      unconfirmed <- wallet.getUnconfirmedBalance()
      reservedUtxos <- wallet.utxoHandling.getUtxos(TxoState.Reserved)
    } yield {
      import org.bitcoins.core.currency.currencyUnitNumeric
      val reserved = reservedUtxos.map(_.output.value).sum
      val total = confirmed + unconfirmed + reserved

      GetBalancesResponse(
        confirmed = formatCurrencyUnit(confirmed, in.isSats),
        unconfirmed = formatCurrencyUnit(unconfirmed, in.isSats),
        reserved = formatCurrencyUnit(reserved, in.isSats),
        total = formatCurrencyUnit(total, in.isSats)
      )
    }
  }

  override def getUtxos(in: GetUtxosRequest): Future[GetUtxosResponse] = {
    wallet.utxoHandling.getUtxos().map { utxos =>
      GetUtxosResponse(utxos = utxos.map(toUtxo))
    }
  }

  override def getReservedUtxos(
      in: GetReservedUtxosRequest): Future[GetUtxosResponse] = {
    wallet.utxoHandling.getUtxos(TxoState.Reserved).map { utxos =>
      GetUtxosResponse(utxos = utxos.map(toUtxo))
    }
  }

  override def getAddresses(
      in: GetAddressesRequest): Future[GetAddressesResponse] = {
    wallet.addressHandling.getAddresses().map { addresses =>
      GetAddressesResponse(addresses = addresses.map(_.address.value))
    }
  }

  override def getSpentAddresses(
      in: GetSpentAddressesRequest): Future[GetAddressesResponse] = {
    wallet.addressHandling.getSpentAddresses().map { addresses =>
      GetAddressesResponse(addresses = addresses.map(_.address.value))
    }
  }

  override def getFundedAddresses(
      in: GetFundedAddressesRequest): Future[GetFundedAddressesResponse] = {
    wallet.addressHandling.getFundedAddresses().map { addressAndValues =>
      val fundedAddresses = addressAndValues.map { case (addressDb, value) =>
        FundedAddress(address = addressDb.address.value,
                      valueSats = value.satoshis.toLong)
      }

      GetFundedAddressesResponse(fundedAddresses = fundedAddresses)
    }
  }

  override def getUnusedAddresses(
      in: GetUnusedAddressesRequest): Future[GetAddressesResponse] = {
    wallet.addressHandling.getUnusedAddresses().map { addresses =>
      GetAddressesResponse(addresses = addresses.map(_.address.value))
    }
  }

  override def getAccounts(
      in: GetAccountsRequest): Future[GetAccountsResponse] = {
    wallet.accountHandling.getAccounts().map { accounts =>
      GetAccountsResponse(xpubs = accounts.map(_.xpub.toString))
    }
  }

  override def getAddressLabels(
      in: GetAddressLabelsRequest): Future[GetAddressLabelsResponse] = {
    wallet.addressHandling.getAddressTags().map { allTags =>
      val labels = allTags
        .groupBy(_.address)
        .map { case (address, addressTags) =>
          AddressLabels(address = address.value,
                        labels = addressTags.map(_.tagName.name))
        }
        .toVector
      GetAddressLabelsResponse(addressLabels = labels)
    }
  }

  // ── New endpoints ──────────────────────────────────────────────────────────

  override def getWalletInfo(
      in: GetWalletInfoRequest): Future[GetWalletInfoResponse] = {
    wallet.getInfo().map { info =>
      GetWalletInfoResponse(
        walletName = info.walletName,
        rootXpub = info.rootXpub.toString,
        xpub = info.xpub.toString,
        hdAccount = info.hdAccount.toString,
        height = info.height,
        blockHash = info.blockHash.hex,
        rescan = info.rescan,
        imported = info.imported
      )
    }
  }

  override def getNewAddress(
      in: GetNewAddressRequest): Future[GetNewAddressResponse] = {
    val addressF = in.label match {
      case Some(label) if label.nonEmpty =>
        val tag = org.bitcoins.core.wallet.utxo.AddressLabelTag(label)
        wallet.addressHandling.getNewAddress(Vector(tag))
      case _ =>
        wallet.addressHandling.getNewAddress()
    }
    addressF.map(addr => GetNewAddressResponse(address = addr.value))
  }

  override def getTransaction(
      in: GetTransactionRequest): Future[GetTransactionResponse] = {
    val txId = DoubleSha256DigestBE.fromHex(in.txid)
    wallet.transactionProcessing.findByTxId(txId).map {
      case None => GetTransactionResponse(txHex = None)
      case Some(txDb) =>
        GetTransactionResponse(txHex = Some(txDb.transaction.hex))
    }
  }

  override def lockUnspent(
      in: LockUnspentRequest): Future[LockUnspentResponse] = {
    val outPoints = in.outpoints.map { op =>
      TransactionOutPoint(DoubleSha256DigestBE.fromHex(op.txid),
                          UInt32(op.vout))
    }.toVector

    val func: Vector[SpendingInfoDb] => Future[Vector[SpendingInfoDb]] =
      utxos =>
        if (in.unlock) wallet.utxoHandling.unmarkUTXOsAsReserved(utxos)
        else wallet.utxoHandling.markUTXOsAsReserved(utxos)

    for {
      utxos <-
        if (in.unlock) wallet.utxoHandling.getUtxos(TxoState.Reserved)
        else wallet.utxoHandling.getUtxos()
      filtered =
        if (outPoints.nonEmpty)
          utxos.filter(u => outPoints.exists(_ == u.outPoint))
        else utxos
      reserved <- func(filtered)
    } yield LockUnspentResponse(success = reserved.nonEmpty)
  }

  override def labelAddress(
      in: LabelAddressRequest): Future[LabelAddressResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    val tag =
      org.bitcoins.core.wallet.utxo.AddressLabelTag(in.label)
    wallet.addressHandling.tagAddress(address, tag).map { tagDb =>
      LabelAddressResponse(
        message =
          s"Added label '${tagDb.tagName.name}' to ${tagDb.address.value}")
    }
  }

  override def getAddressTags(
      in: GetAddressTagsRequest): Future[GetAddressTagsResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    wallet.addressHandling.getAddressTags(address).map { tagDbs =>
      GetAddressTagsResponse(tags = tagDbs.map(_.tagName.name))
    }
  }

  override def getAddressLabel(
      in: GetAddressLabelRequest): Future[GetAddressLabelResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    wallet.addressHandling
      .getAddressTags(address, AddressLabelTagType)
      .map { tagDbs =>
        GetAddressLabelResponse(labels = tagDbs.map(_.tagName.name))
      }
  }

  override def dropAddressLabel(
      in: DropAddressLabelRequest): Future[DropAddressLabelResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    val tagName = AddressLabelTagName(in.label)
    wallet.addressHandling.dropAddressTagName(address, tagName).map { n =>
      DropAddressLabelResponse(message = handleTagDropped(n))
    }
  }

  override def dropAddressLabels(
      in: DropAddressLabelsRequest): Future[DropAddressLabelsResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    wallet.addressHandling
      .dropAddressTagType(address, AddressLabelTagType)
      .map { n =>
        DropAddressLabelsResponse(message = handleTagDropped(n))
      }
  }

  override def getAddressInfo(
      in: GetAddressInfoRequest): Future[GetAddressInfoResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    wallet.addressHandling.getAddressInfo(address).map {
      case Some(info) =>
        GetAddressInfoResponse(pubkey = Some(info.pubkey.hex),
                               hdPath = Some(info.path.toString))
      case None =>
        GetAddressInfoResponse(pubkey = None, hdPath = None)
    }
  }

  override def createNewAccount(
      in: CreateNewAccountRequest): Future[CreateNewAccountResponse] = {
    val purpose =
      org.bitcoins.core.hd.HDPurpose.fromString(in.purpose)
    for {
      _ <- wallet.accountHandling.createNewAccount(purpose)
      accounts <- wallet.accountHandling.getAccounts()
    } yield CreateNewAccountResponse(xpubs = accounts.map(_.xpub.toString))
  }

  override def sendToAddress(
      in: SendToAddressRequest): Future[SendToAddressResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    val amount = Bitcoins(Satoshis(in.amountSats))
    val feeRateOpt = toSatoshisPerVbOpt(in.feeRateSatsPerVbyte)
    for {
      tx <- wallet.sendFundsHandling.sendToAddress(address, amount, feeRateOpt)
      hexStr <-
        if (in.noBroadcast) Future.successful(tx.hex)
        else wallet.broadcastTransaction(tx).map(_ => tx.txIdBE.hex)
    } yield SendToAddressResponse(hex = hexStr)
  }

  override def sendFromOutPoints(
      in: SendFromOutPointsRequest): Future[SendFromOutPointsResponse] = {
    val outPoints = in.outpoints.map { op =>
      TransactionOutPoint(DoubleSha256DigestBE.fromHex(op.txid),
                          UInt32(op.vout))
    }.toVector
    val address = BitcoinAddress.fromString(in.address)
    val amount = Bitcoins(Satoshis(in.amountSats))
    val feeRateOpt = toSatoshisPerVbOpt(in.feeRateSatsPerVbyte)
    for {
      tx <- wallet.sendFundsHandling.sendFromOutPoints(outPoints,
                                                       address,
                                                       amount,
                                                       feeRateOpt)
      _ <- wallet.broadcastTransaction(tx)
    } yield SendFromOutPointsResponse(txid = tx.txIdBE.hex)
  }

  override def sweepWallet(
      in: SweepWalletRequest): Future[SweepWalletResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    val feeRateOpt = toSatoshisPerVbOpt(in.feeRateSatsPerVbyte)
    for {
      tx <- wallet.sendFundsHandling.sweepWallet(address, feeRateOpt)
      _ <- wallet.broadcastTransaction(tx)
    } yield SweepWalletResponse(txid = tx.txIdBE.hex)
  }

  override def sendWithAlgo(
      in: SendWithAlgoRequest): Future[SendWithAlgoResponse] = {
    val address = BitcoinAddress.fromString(in.address)
    val amount = Bitcoins(Satoshis(in.amountSats))
    val feeRateOpt = toSatoshisPerVbOpt(in.feeRateSatsPerVbyte)
    val algo = CoinSelectionAlgo.fromString(in.algo)
    for {
      tx <-
        wallet.sendFundsHandling.sendWithAlgo(address, amount, feeRateOpt, algo)
      _ <- wallet.broadcastTransaction(tx)
    } yield SendWithAlgoResponse(txid = tx.txIdBE.hex)
  }

  override def signPsbt(in: SignPsbtRequest): Future[SignPsbtResponse] = {
    val psbt = PSBT.fromBase64(in.psbtBase64)
    wallet.sendFundsHandling.signPSBT(psbt).map { signed =>
      SignPsbtResponse(signedPsbtBase64 = signed.base64)
    }
  }

  override def opReturnCommit(
      in: OpReturnCommitRequest): Future[OpReturnCommitResponse] = {
    val feeRateOpt = toSatoshisPerVbOpt(in.feeRateSatsPerVbyte)
    for {
      tx <- wallet.sendFundsHandling.makeOpReturnCommitment(in.message,
                                                            in.hashMessage,
                                                            feeRateOpt)
      _ <- wallet.broadcastTransaction(tx)
    } yield OpReturnCommitResponse(txid = tx.txIdBE.hex)
  }

  override def bumpFeeRbf(in: BumpFeeRbfRequest): Future[BumpFeeRbfResponse] = {
    val txId = DoubleSha256DigestBE.fromHex(in.txid)
    val feeRate = SatoshisPerVirtualByte(Satoshis(in.feeRateSatsPerVbyte))
    for {
      tx <- wallet.sendFundsHandling.bumpFeeRBF(txId, feeRate)
      _ <- wallet.broadcastTransaction(tx)
    } yield BumpFeeRbfResponse(txid = tx.txIdBE.hex)
  }

  override def bumpFeeCpfp(
      in: BumpFeeCpfpRequest): Future[BumpFeeCpfpResponse] = {
    val txId = DoubleSha256DigestBE.fromHex(in.txid)
    val feeRate = SatoshisPerVirtualByte(Satoshis(in.feeRateSatsPerVbyte))
    for {
      tx <- wallet.sendFundsHandling.bumpFeeCPFP(txId, feeRate)
      _ <- wallet.broadcastTransaction(tx)
    } yield BumpFeeCpfpResponse(txid = tx.txIdBE.hex)
  }

  override def rescan(in: RescanRequest): Future[RescanResponse] = {
    val startOpt = in.startBlock.flatMap(parseBlockStampOpt)
    val endOpt = in.endBlock.flatMap(parseBlockStampOpt)
    val batchSize =
      in.batchSize.getOrElse(wallet.rescanHandling.discoveryBatchSize())
    wallet.rescanHandling
      .rescanNeutrinoWallet(
        startOpt = startOpt,
        endOpt = endOpt,
        addressBatchSize = batchSize,
        useCreationTime = !in.ignoreCreationTime,
        force = false
      )
      .map { state =>
        import org.bitcoins.core.wallet.rescan.RescanState
        val msg = state match {
          case RescanState.RescanAlreadyStarted |
              _: RescanState.RescanStarted =>
            "Rescan started."
          case RescanState.RescanDone | RescanState.RescanNotNeeded =>
            "Rescan done."
        }
        RescanResponse(message = msg)
      }
  }

  override def sendRawTransaction(
      in: SendRawTransactionRequest): Future[SendRawTransactionResponse] = {
    val tx = Transaction.fromHex(in.txHex)
    wallet.broadcastTransaction(tx).map { _ =>
      SendRawTransactionResponse(txid = tx.txIdBE.hex)
    }
  }

  override def estimateFee(
      in: EstimateFeeRequest): Future[EstimateFeeResponse] = {
    wallet
      .getFeeRate()
      .recover { case scala.util.control.NonFatal(_) =>
        SatoshisPerVirtualByte.negativeOne
      }
      .map { feeRate =>
        EstimateFeeResponse(satsPerVbyte =
          feeRate.toSatsPerVByte.toLong.toDouble)
      }
  }

  // ── Key-manager / wallet-lifecycle operations ──────────────────────────────

  private def kmConf: KeyManagerAppConfig = lifecycle.kmConf

  private def invalidArgument(msg: String): Throwable =
    Status.INVALID_ARGUMENT.withDescription(msg).asRuntimeException()

  private def failedPrecondition(msg: String): Throwable =
    Status.FAILED_PRECONDITION.withDescription(msg).asRuntimeException()

  /** Runs `f`, mapping [[IllegalArgumentException]]s to INVALID_ARGUMENT and
    * any other failure to FAILED_PRECONDITION (e.g. wrong password, seed
    * already backed up, seed missing).
    */
  private def attempt[T](f: => T): Future[T] =
    Try(f) match {
      case Success(v)                                 => Future.successful(v)
      case Failure(e: io.grpc.StatusRuntimeException) => Future.failed(e)
      case Failure(e: IllegalArgumentException) =>
        Future.failed(invalidArgument(e.getMessage))
      case Failure(e) =>
        Future.failed(failedPrecondition(String.valueOf(e.getMessage)))
    }

  private def passwordOpt(opt: Option[String]): Option[AesPassword] =
    opt.filter(_.nonEmpty).map(AesPassword.fromString)

  private def walletNameOpt(opt: Option[String]): Option[String] = {
    val nameOpt = opt.filter(_.nonEmpty)
    nameOpt.foreach { name =>
      if (name.length > WalletNames.walletNameMaxLen)
        throw new IllegalArgumentException(
          s"Invalid wallet name length: ${name.length}. Max length is ${WalletNames.walletNameMaxLen}.")
      if (!WalletNames.validateWalletName(name))
        throw new IllegalArgumentException(s"Invalid wallet name `$name`.")
    }
    nameOpt
  }

  private def seedPath(walletName: Option[String]): Path = {
    val prefix = walletName
      .map(_ + "-")
      .getOrElse(KeyManagerAppConfig.DEFAULT_WALLET_NAME)
    kmConf.seedFolder.resolve(prefix + WalletStorage.ENCRYPTED_SEED_FILE_NAME)
  }

  private def genesisTime: Instant =
    Instant.ofEpochSecond(WalletStorage.GENESIS_TIME)

  override def keyManagerPassphraseChange(in: KeyManagerPassphraseChangeRequest)
      : Future[KeyManagerPassphraseChangeResponse] = attempt {
    WalletStorage.changeAesPassword(
      kmConf.seedPath,
      Some(AesPassword.fromString(in.oldPassword)),
      Some(AesPassword.fromString(in.newPassword)))
    KeyManagerPassphraseChangeResponse()
  }

  override def keyManagerPassphraseSet(in: KeyManagerPassphraseSetRequest)
      : Future[KeyManagerPassphraseSetResponse] = attempt {
    WalletStorage.changeAesPassword(kmConf.seedPath,
                                    None,
                                    Some(AesPassword.fromString(in.password)))
    KeyManagerPassphraseSetResponse()
  }

  override def importSeed(in: ImportSeedRequest): Future[ImportSeedResponse] =
    attempt {
      val path = seedPath(walletNameOpt(in.walletName))
      val mnemonic = MnemonicCode.fromWords(
        in.mnemonic.trim.split("\\s+").toVector.filter(_.nonEmpty))
      val decrypted = DecryptedMnemonic(mnemonic,
                                        genesisTime,
                                        backupTimeOpt = None,
                                        imported = true)
      val state = passwordOpt(in.password) match {
        case Some(pass) => decrypted.encrypt(pass)
        case None       => decrypted
      }
      if (Files.exists(path)) {
        // mirrors the HTTP importseed behavior: never overwrite an existing seed
        ImportSeedResponse(success = false)
      } else {
        WalletStorage.writeSeedToDisk(path, state)
        ImportSeedResponse(success = true)
      }
    }

  override def exportSeed(in: ExportSeedRequest): Future[ExportSeedResponse] =
    attempt {
      val path = seedPath(walletNameOpt(in.walletName))
      val words = WalletStorage
        .readDecryptedSeedPhraseForBackup(path, passwordOpt(in.password))
        .get
      ExportSeedResponse(mnemonic = words.mkString(" "))
    }

  override def markSeedAsBackedUp(
      in: MarkSeedAsBackedUpRequest): Future[MarkSeedAsBackedUpResponse] =
    attempt {
      val path = seedPath(walletNameOpt(in.walletName))
      WalletStorage.markSeedAsBackedUp(path, passwordOpt(in.password)).get
      MarkSeedAsBackedUpResponse(message = "ok")
    }

  override def getSeedBackupTime(
      in: GetSeedBackupTimeRequest): Future[GetSeedBackupTimeResponse] =
    attempt {
      val path = seedPath(walletNameOpt(in.walletName))
      val backupTimeOpt =
        WalletStorage.getSeedBackupTime(path, passwordOpt(in.password)).get
      GetSeedBackupTimeResponse(backupEpochSeconds =
        backupTimeOpt.map(_.getEpochSecond))
    }

  override def importXprv(in: ImportXprvRequest): Future[ImportXprvResponse] =
    attempt {
      val path = seedPath(walletNameOpt(in.walletName))
      val xprv = ExtPrivateKey.fromString(in.xprv)
      val decrypted = DecryptedExtPrivKey(xprv,
                                          genesisTime,
                                          backupTimeOpt = None,
                                          imported = true)
      val state = passwordOpt(in.password) match {
        case Some(pass) => decrypted.encrypt(pass)
        case None       => decrypted
      }
      WalletStorage.writeSeedToDisk(path, state)
      ImportXprvResponse()
    }

  override def listWallets(
      in: ListWalletsRequest): Future[ListWalletsResponse] = attempt {
    val names = Option(kmConf.seedFolder.toFile.list()).toVector
      .flatMap(_.toVector)
      .flatMap { fn =>
        fn.split(WalletStorage.ENCRYPTED_SEED_FILE_NAME).headOption.flatMap {
          wn =>
            if (wn.endsWith("-")) Some(wn.substring(0, wn.length - 1))
            else None
        }
      }
    ListWalletsResponse(walletNames = names)
  }

  override def loadWallet(in: LoadWalletRequest): Future[LoadWalletResponse] = {
    Try((walletNameOpt(in.walletName), passwordOpt(in.aesPassword))) match {
      case Failure(e) =>
        Future.failed(invalidArgument(String.valueOf(e.getMessage)))
      case Success((nameOpt, passOpt)) =>
        lifecycle.loadWallet(nameOpt, passOpt).map { _ =>
          LoadWalletResponse(walletName =
            nameOpt.getOrElse(KeyManagerAppConfig.DEFAULT_WALLET_NAME))
        }
    }
  }
}
