package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.api.wallet.{AddUtxoError, AddUtxoSuccess}
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.wallet._

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

/** Provides functionality for processing transactions. This
  * includes importing UTXOs spent to our wallet, updating
  * confirmation counts and marking UTXOs as spent when
  * spending from our wallet
  */
private[wallet] trait TransactionProcessing extends WalletLogger {
  self: Wallet =>
  /////////////////////
  // Public facing API

  /** @inheritdoc */
  override def processTransaction(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Wallet] = {
    for {
      result <- processTransactionImpl(transaction, blockHashOpt, Vector.empty)
    } yield {
      logger.debug(
        s"Finished processing of transaction=${transaction.txIdBE}. Relevant incomingTXOs=${result.updatedIncoming.length}, outgoingTXOs=${result.updatedOutgoing.length}")
      this
    }
  }

  override def processBlock(block: Block): Future[Wallet] = {
    logger.info(s"Processing block=${block.blockHeader.hash.flip}")
    val start = TimeUtil.currentEpochMs
    val resF = for {
      newWallet <- block.transactions.foldLeft(Future.successful(this)) {
        (acc, transaction) =>
          for {
            _ <- acc
            newWallet <-
              processTransaction(transaction, Some(block.blockHeader.hash.flip))
          } yield {
            newWallet
          }
      }
    } yield newWallet

    val f = for {
      res <- resF

      hash = block.blockHeader.hashBE
      height <- chainQueryApi.getBlockHeight(hash)
      _ <- stateDescriptorDAO.updateSyncHeight(hash, height.get)
    } yield res

    f.onComplete(failure =>
      signalBlockProcessingCompletion(block.blockHeader.hash, failure))

    f.foreach { _ =>
      val stop = TimeUtil.currentEpochMs
      logger.info(
        s"Finished processing of block=${block.blockHeader.hash.flip}. It took ${stop - start}ms")
    }
    f.failed.foreach(e =>
      logger.error(s"Error processing of block=${block.blockHeader.hash.flip}.",
                   e))
    f
  }

  override def findTransaction(
      txId: DoubleSha256DigestBE): Future[Option[TransactionDb]] = {
    transactionDAO.findByTxId(txId)
  }

  override def listTransactions(): Future[Vector[TransactionDb]] =
    transactionDAO.findAll()

  private[wallet] case class ProcessTxResult(
      updatedIncoming: Vector[SpendingInfoDb],
      updatedOutgoing: Vector[SpendingInfoDb])

  /////////////////////
  // Internal wallet API

  protected def insertTransaction(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[TransactionDb] = {
    val txDb = TransactionDbHelper.fromTransaction(tx, blockHashOpt)
    transactionDAO.upsert(txDb)
  }

  private[wallet] def insertOutgoingTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[
    (TransactionDb, OutgoingTransactionDb)] = {
    val outgoingDb =
      OutgoingTransactionDb.fromTransaction(transaction,
                                            inputAmount,
                                            sentAmount,
                                            feeRate.calc(transaction))
    for {
      txDb <- insertTransaction(transaction, blockHashOpt)
      written <- outgoingTxDAO.upsert(outgoingDb)
    } yield (txDb, written)
  }

  /** Processes TXs originating from our wallet.
    * This is called right after we've signed a TX,
    * updating our UTXO state.
    */
  private[wallet] def processOurTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag]): Future[ProcessTxResult] = {
    logger.info(
      s"Processing TX from our wallet, transaction=${transaction.txIdBE} with blockHash=$blockHashOpt")
    for {
      (txDb, _) <-
        insertOutgoingTransaction(transaction,
                                  feeRate,
                                  inputAmount,
                                  sentAmount,
                                  blockHashOpt)
      result <- processTransactionImpl(txDb.transaction, blockHashOpt, newTags)
    } yield {
      val txid = txDb.transaction.txIdBE
      val changeOutputs = result.updatedIncoming.length
      val spentOutputs = result.updatedOutgoing.length

      logger.info(
        s"Processing of internal transaction=$txid resulted in changeOutputs=$changeOutputs and spentUTXOs=$spentOutputs")
      result
    }
  }

  /////////////////////
  // Private methods

  private var blockProcessingSignals =
    Map.empty[DoubleSha256Digest, Promise[DoubleSha256Digest]]

  private[wallet] def subscribeForBlockProcessingCompletionSignal(
      blockHash: DoubleSha256Digest): Future[DoubleSha256Digest] =
    synchronized {
      blockProcessingSignals.get(blockHash) match {
        case Some(existingSignal) => existingSignal.future
        case None =>
          val newSignal = Promise[DoubleSha256Digest]()
          blockProcessingSignals =
            blockProcessingSignals.updated(blockHash, newSignal)
          newSignal.future
      }
    }

  private def signalBlockProcessingCompletion(
      blockHash: DoubleSha256Digest,
      failure: Try[_]): Unit =
    synchronized {
      logger.debug(s"Updating wallet signal completion for ${blockHash}")
      blockProcessingSignals.get(blockHash).foreach { signal =>
        blockProcessingSignals =
          blockProcessingSignals.filterNot(_._1 == blockHash)
        failure match {
          case Success(_) =>
            signal.success(blockHash)
          case Failure(exception) => signal.failure(exception)
        }
      }
    }

  protected def processIncomingUtxos(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag]): Future[Vector[SpendingInfoDb]] =
    spendingInfoDAO
      .findTx(transaction)
      .flatMap {
        // no existing elements found
        case Vector() =>
          processNewIncomingTx(transaction, blockHashOpt, newTags)
            .map(_.toVector)

        case txos: Vector[SpendingInfoDb] =>
          val processedVec = txos.map { txo =>
            processExistingIncomingTxo(transaction, blockHashOpt, txo)
          }
          Future.sequence(processedVec)
      }

  protected def processOutgoingUtxos(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[
    Vector[SpendingInfoDb]] = {
    for {
      outputsBeingSpent <- spendingInfoDAO.findOutputsBeingSpent(transaction)

      _ <-
        if (outputsBeingSpent.nonEmpty)
          insertTransaction(transaction, blockHashOpt)
        else Future.unit

      // unreserved outputs now they are in a block
      outputsToUse = blockHashOpt match {
        case Some(_) =>
          outputsBeingSpent.map { out =>
            if (out.state == TxoState.Reserved)
              out.copyWithState(TxoState.PendingConfirmationsReceived)
            else out
          }
        case None =>
          outputsBeingSpent
      }

      processed <- Future
        .sequence {
          outputsToUse.map(markAsSpent(_, transaction.txIdBE))
        }
        .map(_.toVector)
    } yield processed.flatten

  }

  /** Does the grunt work of processing a TX.
    * This is called by either the internal or public TX
    * processing method, which logs and transforms the
    * output fittingly.
    */
  private def processTransactionImpl(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag]): Future[ProcessTxResult] = {

    logger.debug(
      s"Processing transaction=${transaction.txIdBE} with blockHash=$blockHashOpt")

    for {
      incoming <- processIncomingUtxos(transaction, blockHashOpt, newTags)
      outgoing <- processOutgoingUtxos(transaction, blockHashOpt)
      _ <- walletCallbacks.executeOnTransactionProcessed(logger, transaction)
    } yield {
      ProcessTxResult(incoming, outgoing)
    }
  }

  /** If the given UTXO is marked as unspent, updates
    * its spending status. Otherwise returns an error.
    */
  private def markAsSpent(
      out: SpendingInfoDb,
      spendingTxId: DoubleSha256DigestBE): Future[Option[SpendingInfoDb]] = {
    out.state match {
      case ConfirmedReceived | PendingConfirmationsReceived |
          BroadcastReceived =>
        val updated =
          out
            .copyWithState(state = BroadcastSpent)
            .copyWithSpendingTxId(spendingTxId)
        val updatedF =
          spendingInfoDAO.update(updated)
        updatedF.foreach(updated =>
          logger.debug(
            s"Marked utxo=${updated.toHumanReadableString} as state=${updated.state}"))
        updatedF.map(Some(_))
      case TxoState.Reserved | TxoState.PendingConfirmationsSpent |
          BroadcastSpent =>
        val updated =
          out.copyWithSpendingTxId(spendingTxId)
        val updatedF =
          spendingInfoDAO.update(updated)
        updatedF.map(Some(_))
      case TxoState.ImmatureCoinbase =>
        Future.failed(new RuntimeException(
          s"Attempting to spend an ImmatureCoinbase ${out.outPoint.hex}, this should not be possible until it is confirmed."))
      case TxoState.ConfirmedSpent =>
        Future.failed(new RuntimeException(
          s"Attempted to mark an already spent utxo ${out.outPoint.hex} with a new spending tx ${spendingTxId.hex}"))
      case TxoState.DoesNotExist =>
        Future.failed(new RuntimeException(
          s"Attempted to process a transaction for a utxo that does not exist ${out.outPoint.hex} with a new spending tx ${spendingTxId.hex}"))
    }
  }

  /** Inserts the UTXO at the given index into our DB, swallowing the
    * error if any (this is because we're operating on data we've
    * already verified).
    */
  private def processUtxo(
      transaction: Transaction,
      index: Int,
      state: TxoState): Future[SpendingInfoDb] = {
    addUtxo(transaction = transaction, vout = UInt32(index), state = state)
      .flatMap {
        case AddUtxoSuccess(utxo) => Future.successful(utxo)
        case err: AddUtxoError =>
          logger.error(s"Could not add UTXO", err)
          Future.failed(err)
      }
  }

  private case class OutputWithIndex(output: TransactionOutput, index: Int)

  /** Processes an incoming transaction that already exists in our wallet.
    * If the incoming transaction has more confirmations than what we
    * have in the DB, we update the TX
    */
  private def processExistingIncomingTxo(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      foundTxo: SpendingInfoDb): Future[SpendingInfoDb] = {
    if (foundTxo.txid != transaction.txIdBE) {
      val errMsg =
        Seq(
          s"Found TXO has txid=${foundTxo.txid}, tx we were given has txid=${transaction.txIdBE}.",
          "This is either a reorg or a double spent, which is not implemented yet"
        ).mkString(" ")
      logger.error(errMsg)
      Future.failed(new RuntimeException(errMsg))
    } else {
      blockHashOpt match {
        case Some(blockHash) =>
          logger.debug(
            s"Updating block_hash of txo=${transaction.txIdBE}, new block hash=$blockHash")

          // If the utxo was marked reserved we want to update it to spent now
          // since it has been included in a block
          val unreservedTxo = foundTxo.state match {
            case TxoState.Reserved =>
              foundTxo.copyWithState(TxoState.PendingConfirmationsSpent)
            case TxoState.PendingConfirmationsReceived |
                TxoState.ConfirmedReceived |
                TxoState.PendingConfirmationsSpent | TxoState.ConfirmedSpent |
                TxoState.DoesNotExist | TxoState.ImmatureCoinbase |
                BroadcastReceived | BroadcastSpent =>
              foundTxo
          }

          val updateTxDbF = insertTransaction(transaction, blockHashOpt)

          // Update Txo State
          updateTxDbF.flatMap(_ =>
            updateUtxoConfirmedState(unreservedTxo).flatMap {
              case Some(txo) =>
                logger.debug(
                  s"Updated block_hash of txo=${txo.txid.hex} new block hash=${blockHash.hex}")
                Future.successful(txo)
              case None =>
                // State was not updated so we need to update it so it's block hash is in the database
                spendingInfoDAO.update(unreservedTxo)
            })
        case None =>
          logger.debug(
            s"Skipping further processing of transaction=${transaction.txIdBE.hex}, already processed.")
          Future.successful(foundTxo)
      }
    }
  }

  private def addUTXOsFut(
      outputsWithIndex: Seq[OutputWithIndex],
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[
    Seq[SpendingInfoDb]] = {
    val stateF: Future[TxoState] = blockHashOpt match {
      case None =>
        Future.successful(TxoState.BroadcastReceived)
      case Some(blockHash) =>
        chainQueryApi.getNumberOfConfirmations(blockHash).map {
          case None =>
            TxoState.PendingConfirmationsReceived
          case Some(confs) =>
            if (transaction.isCoinbase && confs <= Consensus.coinbaseMaturity) {
              TxoState.ImmatureCoinbase
            } else if (confs >= walletConfig.requiredConfirmations) {
              TxoState.ConfirmedReceived
            } else {
              TxoState.PendingConfirmationsReceived
            }
        }
    }

    stateF.flatMap { state =>
      val outputsVec = outputsWithIndex.map { out =>
        processUtxo(
          transaction,
          out.index,
          state = state
        )
      }
      Future.sequence(outputsVec)
    }
  }

  private[wallet] def insertIncomingTransaction(
      transaction: Transaction,
      incomingAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[
    (TransactionDb, IncomingTransactionDb)] = {
    val incomingDb = IncomingTransactionDb(transaction.txIdBE, incomingAmount)
    for {
      txDb <- insertTransaction(transaction, blockHashOpt)
      written <- incomingTxDAO.upsert(incomingDb)
    } yield (txDb, written)
  }

  private def getRelevantOutputs(
      transaction: Transaction): Future[Seq[OutputWithIndex]] = {
    val spks = transaction.outputs.map(_.scriptPubKey)
    scriptPubKeyDAO.findScriptPubKeys(spks.toVector).map { addrs =>
      val withIndex =
        transaction.outputs.zipWithIndex
      withIndex.collect {
        case (out, idx)
            if addrs.map(_.scriptPubKey).contains(out.scriptPubKey) =>
          OutputWithIndex(out, idx)
      }
    }
  }

  /** Processes an incoming transaction that's new to us
    *
    * @return A list of inserted transaction outputs
    */
  private def processNewIncomingTx(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag]): Future[Seq[SpendingInfoDb]] = {
    getRelevantOutputs(transaction).flatMap {
      case Nil =>
        logger.debug(
          s"Found no outputs relevant to us in transaction${transaction.txIdBE}")
        Future.successful(Vector.empty)

      case outputsWithIndex =>
        val count = outputsWithIndex.length
        val outputStr = {
          outputsWithIndex
            .map { elem =>
              s"${transaction.txIdBE.hex}:${elem.index}"
            }
            .mkString(", ")
        }
        logger.trace(
          s"Found $count relevant output(s) in transaction=${transaction.txIdBE.hex}: $outputStr")

        val totalIncoming = outputsWithIndex.map(_.output.value).sum

        val spks = outputsWithIndex.map(_.output.scriptPubKey)
        val spksInDbF = addressDAO.findByScriptPubKeys(spks.toVector)

        val ourOutputsF = for {
          spksInDb <- spksInDbF
        } yield {
          outputsWithIndex.collect {
            case OutputWithIndex(out, idx)
                if spksInDb.map(_.scriptPubKey).contains(out.scriptPubKey) =>
              OutputWithIndex(out, idx)
          }
        }

        val txDbF: Future[(TransactionDb, IncomingTransactionDb)] =
          insertIncomingTransaction(transaction, totalIncoming, blockHashOpt)

        val prevTagsDbF = for {
          (txDb, _) <- txDbF
          prevTagDbs <-
            addressTagDAO.findTx(txDb.transaction, networkParameters)
        } yield prevTagDbs

        val newTagsF = for {
          ourOutputs <- ourOutputsF
          prevTagDbs <- prevTagsDbF
          prevTags = prevTagDbs.map(_.addressTag)
          tagsToUse = prevTags
            .filterNot(tag => newTags.contains(tag)) ++ newTags
          newTagDbs = ourOutputs.flatMap { out =>
            val address = BitcoinAddress
              .fromScriptPubKey(out.output.scriptPubKey, networkParameters)
            tagsToUse.map(tag => AddressTagDb(address, tag))
          }
          created <- addressTagDAO.createAll(newTagDbs.toVector)
        } yield created

        for {
          (txDb, _) <- txDbF
          ourOutputs <- ourOutputsF
          utxos <- addUTXOsFut(ourOutputs, txDb.transaction, blockHashOpt)
          _ <- newTagsF
        } yield utxos
    }
  }
}
