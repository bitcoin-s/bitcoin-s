package org.bitcoins.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.models._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.ReadMnemonicError.DecryptionError
import org.bitcoins.wallet.ReadMnemonicError.JsonParsingError
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.core.bloom.BloomFilter
import org.bitcoins.core.bloom.BloomUpdateAll
import slick.jdbc.SQLiteProfile
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.wallet.internal.UtxoHandling
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.wallet.internal.AddressAndAccountHandling
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.protocol.transaction.Transaction

abstract class LockedWallet
    extends LockedWalletApi
    with UtxoHandling
    with AddressHandling
    with AccountHandling
    with BitcoinSLogger {

  private[wallet] val addressDAO: AddressDAO = AddressDAO()
  private[wallet] val accountDAO: AccountDAO = AccountDAO()
  private[wallet] val utxoDAO: SpendingInfoDAO = SpendingInfoDAO()
  private[wallet] val incomingTxoDAO = IncomingTxoDAO(SQLiteProfile)
  private[wallet] val outgoingTxoDAO = OutgoingTxoDAO(SQLiteProfile)

  /** Sums up the value of all incoming
    * TXs in the wallet, filtered by the given predicate */
  // TODO account for outgoing TXs
  private def filterThenSum(
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
    filterThenSum(_.confirmations > 0)

  // TODO account for outgoing TXs
  override def getUnconfirmedBalance(): Future[CurrencyUnit] =
    filterThenSum(_.confirmations == 0)

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

          val addUTXOsFut: Future[Seq[(SpendingInfoDb, OutputWithIndex)]] =
            Future
              .sequence {
                xs.map(out => processUtxo(transaction, out.index).map(_ -> out))
              }

          val incomingTXOsFut =
            addUTXOsFut.map(createIncomingTxos(transaction, confirmations, _))

          val writeIncomingTXOsFut =
            incomingTXOsFut.flatMap(incomingTxoDAO.createAll)

          writeIncomingTXOsFut

      }
    }
  }

  /**
    * Inserts the UTXO at the given index into our DB, swallowing the
    * error if any (this is because we're operating on data we've
    * already verified). This method is only meant as a helper method in
    * processing transactions.
    */
  // TODO move this into a TX processing trait
  def processUtxo(
      transaction: Transaction,
      index: Int): Future[SpendingInfoDb] =
    addUtxo(transaction, UInt32(index))
      .flatMap {
        case AddUtxoSuccess(utxo) => Future.successful(utxo)
        case err: AddUtxoError =>
          logger.error(s"Could not add UTXO", err)
          Future.failed(err)
      }

  /**
    * @param xs UTXO sequence we want to create
    *           incoming wallet TXOs for
    */
  // TODO move this into a TX processing trait
  private def createIncomingTxos(
      transaction: Transaction,
      confirmations: Int,
      xs: Seq[(SpendingInfoDb, OutputWithIndex)]): Vector[IncomingWalletTXO] = {
    xs.map {
      case (utxo, OutputWithIndex(out, _)) =>
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
      // TODO The assumption here is that double-spends never occur. That's not
      // the case. This must be fixed when double-spend logic is implemented.
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
      // TODO: This is bad in the case of double-spends and re-orgs. Come back
      // and look at this when implementing logic for those scenarios.
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
            val txoProcessingFutures: Vector[
              Future[Option[IncomingWalletTXO]]] = txos
              .map(processExistingIncomingTxo(transaction, confirmations, _))

            Future
              .sequence(txoProcessingFutures)
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
