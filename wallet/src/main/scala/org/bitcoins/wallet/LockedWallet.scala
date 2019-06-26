package org.bitcoins.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.hd._
import org.bitcoins.core.currency._
import org.bitcoins.core.protocol.blockchain._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.{BitcoinSLogger, EitherUtil}
import org.bitcoins.wallet.api.AddUtxoError.{AddressNotFound, BadSPK}
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.models._

import scala.concurrent.Future
import scala.util.Success
import scala.util.Failure
import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.ReadMnemonicError.DecryptionError
import org.bitcoins.wallet.ReadMnemonicError.JsonParsingError
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.bloom.BloomUpdateAll
import slick.jdbc.SQLiteProfile
import org.bitcoins.core.util.FutureUtil

abstract class LockedWallet extends LockedWalletApi with BitcoinSLogger {

  private[wallet] val addressDAO: AddressDAO = AddressDAO()
  private[wallet] val accountDAO: AccountDAO = AccountDAO()
  private[wallet] val utxoDAO: SpendingInfoDAO = SpendingInfoDAO()
  private[wallet] val incomingTxoDAO = IncomingTxoDAO(SQLiteProfile)
  private[wallet] val outgoingTxoDAO = OutgoingTxoDAO(SQLiteProfile)

  /** Sums up the value of all incoming
    * TXs in the wallet, filtered by the given predicate */
  // TODO account for outgoing TXs
  private def sumOfIncomingGiven(
      predicate: IncomingWalletTXO => Boolean): Future[CurrencyUnit] = {
    for (utxos <- incomingTxoDAO.findAllWithSpendingInfo())
      yield
        utxos
          .collect {
            case (txo, spendInfo) if predicate(txo) => spendInfo.value
          }
          .fold(0.sats)(_ + _)
  }

  // TODO account for outgoing TXs
  override def getBalance(): Future[CurrencyUnit] =
    sumOfIncomingGiven(_.confirmations > 0)

  // TODO account for outgoing TXs
  override def getUnconfirmedBalance(): Future[CurrencyUnit] =
    sumOfIncomingGiven(_.confirmations == 0)

  /** The default HD coin */
  private[wallet] lazy val DEFAULT_HD_COIN: HDCoin = {
    val coinType = chainParams match {
      case MainNetChainParams                         => HDCoinType.Bitcoin
      case RegTestNetChainParams | TestNetChainParams => HDCoinType.Testnet
    }
    HDCoin(walletConfig.defaultAccountKind, coinType)
  }

  /**
    * @inheritdoc
    */
  override def unlock(passphrase: AesPassword): UnlockWalletResult = {
    logger.debug(s"Trying to unlock wallet")
    val result = WalletStorage.decryptMnemonicFromDisk(passphrase)
    result match {
      case DecryptionError =>
        logger.error(s"Bad password for unlocking wallet!")
        UnlockWalletError.BadPassword
      case JsonParsingError(message) =>
        logger.error(s"JSON parsing error when unlocking wallet: $message")
        UnlockWalletError.JsonParsingError(message)
      case ReadMnemonicError.NotFoundError =>
        logger.error(s"Encrypted mnemonic not found when unlocking the wallet!")
        UnlockWalletError.MnemonicNotFound

      case ReadMnemonicSuccess(mnemonic) =>
        logger.debug(s"Successfully uunlocked wallet")
        UnlockWalletSuccess(Wallet(mnemonic))
    }
  }

  override def listAccounts(): Future[Vector[AccountDb]] =
    accountDAO.findAll()

  override def listAddresses(): Future[Vector[AddressDb]] =
    addressDAO.findAll()

  /** Enumerates the public keys in this wallet */
  private[wallet] def listPubkeys(): Future[Vector[ECPublicKey]] =
    addressDAO.findAllPubkeys().map(_.toVector)

  /** Gets the size of the bloom filter for this wallet  */
  private def getBloomFilterSize(): Future[Int] = {
    for {
      pubkeys <- listPubkeys()
    } yield {
      // when a public key is inserted into a filter
      // both the pubkey and the hash of the pubkey
      // gets inserted
      pubkeys.length * 2
    }

  }

  // todo: insert TXIDs? need to track which txids we should
  // ask for, somehow
  override def getBloomFilter(): Future[BloomFilter] = {
    for {
      pubkeys <- listPubkeys()
      filterSize <- getBloomFilterSize()
    } yield {

      // todo: Is this the best flag to use?
      val bloomFlag = BloomUpdateAll

      val baseBloom =
        BloomFilter(numElements = filterSize,
                    falsePositiveRate = walletConfig.bloomFalsePositiveRate,
                    flags = bloomFlag)

      pubkeys.foldLeft(baseBloom) { _.insert(_) }
    }
  }

  /**
    * Tries to convert the provided spk to an address, and then checks if we have
    * it in our address table
    */
  private def findAddress(
      spk: ScriptPubKey): Future[Either[AddUtxoError, AddressDb]] =
    BitcoinAddress.fromScriptPubKey(spk, networkParameters) match {
      case Success(address) =>
        addressDAO.findAddress(address).map {
          case Some(addrDb) => Right(addrDb)
          case None         => Left(AddressNotFound)
        }
      case Failure(_) => Future.successful(Left(BadSPK))
    }

  /** Constructs a DB level representation of the given UTXO, and persist it to disk */
  private def writeUtxo(
      output: TransactionOutput,
      outPoint: TransactionOutPoint,
      addressDb: AddressDb): Future[SpendingInfoDb] = {

    val utxo: SpendingInfoDb = addressDb match {
      case segwitAddr: SegWitAddressDb =>
        SegwitV0SpendingInfo(
          id = None,
          outPoint = outPoint,
          output = output,
          privKeyPath = segwitAddr.path,
          scriptWitness = segwitAddr.witnessScript
        )
      case LegacyAddressDb(path, _, _, _, _) =>
        LegacySpendingInfo(outPoint = outPoint,
                           output = output,
                           privKeyPath = path)
      case nested: NestedSegWitAddressDb =>
        throw new IllegalArgumentException(
          s"Bad utxo $nested. Note: nested segwit is not implemented")
    }

    utxoDAO.create(utxo).map { written =>
      val writtenOut = written.outPoint
      logger.info(
        s"Successfully inserted UTXO ${writtenOut.txId.hex}:${writtenOut.vout.toInt} into DB")
      logger.info(s"UTXO details: ${written.output}")
      written
    }
  }

  private case class OutputWithIndex(output: TransactionOutput, index: Int)

  /**
    * Processes an incoming transaction that's new to us
    * @return A list of inserted transaction outputs
    */
  private def processNewIncomingTx(
      transaction: Transaction,
      confirmations: Int): Future[Vector[IncomingWalletTXO]] = {
    addressDAO.findAll().flatMap { addrs =>
      val relevantOutsWithIdx: Seq[OutputWithIndex] = {
        val withIndex =
          transaction.outputs.zipWithIndex
        withIndex.collect {
          case (out, idx)
              if addrs.map(_.scriptPubKey).contains(out.scriptPubKey) =>
            OutputWithIndex(out, idx)
        }
      }

      relevantOutsWithIdx match {
        case Nil =>
          logger.debug(
            s"Found no outputs relevant to us in transaction${transaction.txIdBE}")
          Future.successful(Vector.empty)

        case xs =>
          val count = xs.length
          val outputStr = {
            xs.map { elem =>
                s"${transaction.txIdBE.hex}:${elem.index}"
              }
              .mkString(", ")
          }
          logger.trace(
            s"Found $count relevant output(s) in transaction=${transaction.txIdBE}: $outputStr")
          if (xs.length > 1) {
            logger.warn(
              s"${xs.length} SPKs were relevant to transaction=${transaction.txIdBE}, but we aren't able to handle more than 1 for the time being")
          }

          def processUtxo(index: Int): Future[SpendingInfoDb] =
            addUtxo(transaction, UInt32(index))
              .flatMap {
                case AddUtxoSuccess(utxo) => Future.successful(utxo)
                case err: AddUtxoError =>
                  logger.error(s"Could not add UTXO", err)
                  Future.failed(err)
              }

          val addUTXOsFut: Future[Seq[(SpendingInfoDb, OutputWithIndex)]] =
            Future
              .sequence {
                xs.map(out => processUtxo(out.index).map(_ -> out))
              }

          val createIncomingTxo: Seq[(SpendingInfoDb, OutputWithIndex)] => Vector[
            IncomingWalletTXO] =
            _.map {
              case (utxo, OutputWithIndex(out, index)) =>
                IncomingWalletTXO(
                  confirmations = confirmations,
                  // is this always the case?
                  spent = false,
                  scriptPubKey = out.scriptPubKey,
                  txid = transaction.txIdBE,
                  // always defined, as its freshly
                  // written to the DB
                  spendingInfoID = utxo.id.get
                )
            }.toVector

          val incomingTXOsFut = addUTXOsFut.map(createIncomingTxo)
          val writeIncomingTXOsFut =
            incomingTXOsFut.flatMap(incomingTxoDAO.createAll)

          writeIncomingTXOsFut

      }
    }
  }

  /**
    * Processes an incoming transaction that already exists in our wallet.
    * If the incoming transaction has more confirmations than what we
    * have in the DB, we update the TX
    */
  private def processExistingIncomingTxo(
      transaction: Transaction,
      confirmations: Int,
      foundTxo: IncomingWalletTXO): Future[Option[IncomingWalletTXO]] = {
    if (foundTxo.confirmations < confirmations) {
      logger.debug(
        s"Increasing confirmation count of txo=${transaction.txIdBE}, old=${foundTxo.confirmations} new=${confirmations}")
      val updateF =
        incomingTxoDAO.update(foundTxo.copy(confirmations = confirmations))

      updateF.foreach(tx =>
        logger.debug(
          s"Updated confirmation count=${tx.confirmations} of output=${foundTxo}"))
      updateF.failed.foreach(err =>
        logger.error(
          s"Failed to update confirmation count of transaction=${transaction.txIdBE}",
          err))

      updateF.map(Some(_))
    } else if (foundTxo.confirmations > confirmations) {
      val msg =
        List(
          s"Incoming transaction=${transaction.txIdBE} has fewer confirmations=$confirmations",
          s"than what we already have registered=${foundTxo.confirmations}! I don't know how",
          s"to handle this."
        ).mkString(" ")
      logger.warn(msg)
      Future.failed(new RuntimeException(msg))
    } else {
      logger.debug(
        s"Skipping further processing of transaction=${transaction.txIdBE}, already processed.")
      Future.successful(None)
    }
  }

  override def processTransaction(
      transaction: Transaction,
      confirmations: Int): Future[LockedWallet] = {
    logger.info(
      s"Processing transaction=${transaction.txIdBE} with confirmations=$confirmations")

    val incomingTxoFut: Future[Vector[IncomingWalletTXO]] =
      incomingTxoDAO
        .findTx(transaction)
        .flatMap {
          // no existing elements found
          case Vector() =>
            processNewIncomingTx(transaction, confirmations)

          case txos: Vector[IncomingWalletTXO] =>
            Future
              .sequence {
                txos
                  .map(
                    output =>
                      processExistingIncomingTxo(transaction,
                                                 confirmations,
                                                 output))

              }
              .map(_.flatten)

        }

    val outgoingTxFut: Future[Unit] = {
      logger.warn(s"Skipping processing of outgoing TX state!")
      FutureUtil.unit
    }

    val aggregateFut =
      for {
        incoming <- incomingTxoFut
        _ <- outgoingTxFut
      } yield {
        logger.info(
          s"Finished processing of transaction=${transaction.txIdBE}. Relevant incomingTXOs=${incoming.length}")
        this
      }

    aggregateFut.failed.foreach { err =>
      val msg = s"Error when processing transaction=${transaction.txIdBE}"
      logger.error(msg, err)
    }

    aggregateFut

  }

  /**
    * Adds the provided UTXO to the wallet, making it
    * available for spending.
    */
  private def addUtxo(
      transaction: Transaction,
      vout: UInt32): Future[AddUtxoResult] = {
    import AddUtxoError._
    import org.bitcoins.core.util.EitherUtil.EitherOps._

    logger.info(s"Adding UTXO to wallet: ${transaction.txId.hex}:${vout.toInt}")

    // first check: does the provided vout exist in the tx?
    val voutIndexOutOfBounds: Boolean = {
      val voutLength = transaction.outputs.length
      val outOfBunds = voutLength <= vout.toInt

      if (outOfBunds)
        logger.error(
          s"TX with TXID ${transaction.txId.hex} only has $voutLength, got request to add vout ${vout.toInt}!")
      outOfBunds
    }

    if (voutIndexOutOfBounds) {
      Future.successful(VoutIndexOutOfBounds)
    } else {

      val output = transaction.outputs(vout.toInt)
      val outPoint = TransactionOutPoint(transaction.txId, vout)

      // second check: do we have an address associated with the provided
      // output in our DB?
      val addressDbEitherF: Future[Either[AddUtxoError, AddressDb]] =
        findAddress(output.scriptPubKey)

      // insert the UTXO into the DB
      addressDbEitherF.flatMap { addressDbE =>
        val biasedE: Either[AddUtxoError, Future[SpendingInfoDb]] = for {
          addressDb <- addressDbE
        } yield writeUtxo(output, outPoint, addressDb)

        EitherUtil.liftRightBiasedFutureE(biasedE)
      } map {
        case Right(utxo) => AddUtxoSuccess(utxo)
        case Left(e)     => e
      }
    }
  }

  /**
    * @inheritdoc
    */
  // override def updateUtxo: Future[WalletApi] = ???

  override def listUtxos(): Future[Vector[SpendingInfoDb]] =
    utxoDAO.findAll()

  /**
    * @param account Account to generate address from
    * @param chainType What chain do we generate from? Internal change vs. external
    */
  private def getNewAddressHelper(
      account: AccountDb,
      chainType: HDChainType
  ): Future[BitcoinAddress] = {
    logger.debug(s"Getting new $chainType adddress for ${account.hdAccount}")

    val accountIndex = account.hdAccount.index

    val lastAddrOptF = chainType match {
      case HDChainType.External =>
        addressDAO.findMostRecentExternal(accountIndex)
      case HDChainType.Change =>
        addressDAO.findMostRecentChange(accountIndex)
    }

    lastAddrOptF.flatMap { lastAddrOpt =>
      val addrPath: HDPath = lastAddrOpt match {
        case Some(addr) =>
          val next = addr.path.next
          logger.debug(
            s"Found previous address at path=${addr.path}, next=$next")
          next
        case None =>
          val account = HDAccount(DEFAULT_HD_COIN, accountIndex)
          val chain = account.toChain(chainType)
          val address = HDAddress(chain, 0)
          val path = address.toPath
          logger.debug(s"Did not find previous address, next=$path")
          path
      }

      val addressDb = {
        val pathDiff =
          account.hdAccount.diff(addrPath) match {
            case Some(value) => value
            case None =>
              throw new RuntimeException(
                s"Could not diff ${account.hdAccount} and $addrPath")
          }

        val pubkey = account.xpub.deriveChildPubKey(pathDiff) match {
          case Failure(exception) => throw exception
          case Success(value)     => value.key
        }

        addrPath match {
          case segwitPath: SegWitHDPath =>
            AddressDbHelper
              .getSegwitAddress(pubkey, segwitPath, networkParameters)
          case legacyPath: LegacyHDPath =>
            AddressDbHelper.getLegacyAddress(pubkey,
                                             legacyPath,
                                             networkParameters)
          case nestedPath: NestedSegWitHDPath =>
            AddressDbHelper.getNestedSegwitAddress(pubkey,
                                                   nestedPath,
                                                   networkParameters)
        }
      }
      logger.debug(s"Writing $addressDb to DB")
      val writeF = addressDAO.create(addressDb)
      writeF.foreach { written =>
        logger.debug(
          s"Got ${chainType} address ${written.address} at key path ${written.path} with pubkey ${written.ecPublicKey}")
      }

      writeF.map(_.address)
    }
  }

  /**
    * right now only generates P2WPKH addresses
    *
    * @inheritdoc
    */
  override def getNewAddress(account: AccountDb): Future[BitcoinAddress] = {
    val addrF = getNewAddressHelper(account, HDChainType.External)
    addrF
  }

  override def getAddressInfo(
      address: BitcoinAddress): Future[Option[AddressInfo]] = {

    val addressOptF = addressDAO.findAddress(address)
    addressOptF.map { addressOpt =>
      addressOpt.map { address =>
        AddressInfo(pubkey = address.ecPublicKey,
                    network = address.address.networkParameters,
                    path = address.path)
      }
    }
  }

  /** Generates a new change address */
  override protected[wallet] def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress] = {
    getNewAddressHelper(account, HDChainType.Change)
  }

  /** @inheritdoc */
  override protected[wallet] def getDefaultAccount(): Future[AccountDb] = {
    for {
      account <- accountDAO.read((DEFAULT_HD_COIN, 0))
    } yield
      account.getOrElse(
        throw new RuntimeException(
          s"Could not find account with ${DEFAULT_HD_COIN.purpose.constant} " +
            s"purpose field and ${DEFAULT_HD_COIN.coinType.toInt} coin field"))
  }

}

object LockedWallet {
  private case class LockedWalletImpl()(
      implicit val ec: ExecutionContext,
      val walletConfig: WalletAppConfig)
      extends LockedWallet

  def apply()(
      implicit ec: ExecutionContext,
      config: WalletAppConfig): LockedWallet = LockedWalletImpl()

}
