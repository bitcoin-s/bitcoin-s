package org.bitcoins.wallet.internal

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.wallet._
import org.bitcoins.wallet.api.{AddUtxoError, AddUtxoSuccess}
import org.bitcoins.wallet.models._

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
    val res = block.transactions.foldLeft(Future.successful(this)) {
      (acc, transaction) =>
        for {
          _ <- acc
          newWallet <-
            processTransaction(transaction, Some(block.blockHeader.hash.flip))
        } yield {
          newWallet
        }
    }
    res.onComplete(failure =>
      signalBlockProcessingCompletion(block.blockHeader.hash, failure))
    res.foreach(_ =>
      logger.info(
        s"Finished processing of block=${block.blockHeader.hash.flip}."))
    res.failed.foreach(e =>
      logger.error(s"Error processing of block=${block.blockHeader.hash.flip}.",
                   e))
    res
  }

  override def listTransactions(): Future[Vector[TransactionDb]] =
    transactionDAO.findAll()

  private[wallet] case class ProcessTxResult(
      updatedIncoming: List[SpendingInfoDb],
      updatedOutgoing: List[SpendingInfoDb])

  /////////////////////
  // Internal wallet API

  private[wallet] def insertOutgoingTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit): Future[OutgoingTransactionDb] = {
    val txDb = TransactionDb.fromTransaction(transaction)
    val outgoingDb =
      OutgoingTransactionDb.fromTransaction(transaction,
                                            inputAmount,
                                            sentAmount,
                                            feeRate.calc(transaction))
    for {
      _ <- transactionDAO.upsert(txDb)
      written <- outgoingTxDAO.upsert(outgoingDb)
    } yield written
  }

  /**
    * Processes TXs originating from our wallet.
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
      _ <-
        insertOutgoingTransaction(transaction, feeRate, inputAmount, sentAmount)
      result <- processTransactionImpl(transaction, blockHashOpt, newTags)
    } yield {
      val txid = transaction.txIdBE
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
      aggregate <- {

        val incomingTxoFut: Future[Vector[SpendingInfoDb]] =
          spendingInfoDAO
            .findTx(transaction)
            .flatMap {
              // no existing elements found
              case Vector() =>
                processNewIncomingTx(transaction, blockHashOpt, newTags)
                  .map(_.toVector)

              case txos: Vector[SpendingInfoDb] =>
                FutureUtil
                  .sequentially(txos)(txo =>
                    processExistingIncomingTxo(transaction, blockHashOpt, txo))
                  .map(_.toVector)
            }

        val outgoingTxFut: Future[Vector[SpendingInfoDb]] = {
          for {
            outputsBeingSpent <-
              spendingInfoDAO.findOutputsBeingSpent(transaction)

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

            processed <-
              FutureUtil.sequentially(outputsToUse)(markAsPendingSpent)
          } yield processed.flatten.toVector

        }

        val aggregateFut =
          for {
            incoming <- incomingTxoFut
            outgoing <- outgoingTxFut
            _ <-
              walletCallbacks.executeOnTransactionProcessed(logger, transaction)
          } yield {
            ProcessTxResult(incoming.toList, outgoing.toList)
          }

        aggregateFut.failed.foreach { err =>
          val msg = s"Error when processing transaction=${transaction.txIdBE}"
          logger.error(msg, err)
        }

        aggregateFut
      }
    } yield {
      aggregate
    }
  }

  /** If the given UTXO is marked as unspent, updates
    * its spending status. Otherwise returns `None`.
    */
  private val markAsPendingSpent: SpendingInfoDb => Future[
    Option[SpendingInfoDb]] = { out: SpendingInfoDb =>
    out.state match {
      case TxoState.ConfirmedReceived | TxoState.PendingConfirmationsReceived =>
        val updated =
          out.copyWithState(state = TxoState.PendingConfirmationsSpent)
        val updatedF =
          spendingInfoDAO.update(updated)
        updatedF.foreach(updated =>
          logger.debug(
            s"Marked utxo=${updated.toHumanReadableString} as state=${updated.state}"))
        updatedF.map(Some(_))
      case TxoState.Reserved | TxoState.ConfirmedSpent |
          TxoState.PendingConfirmationsSpent | TxoState.DoesNotExist =>
        FutureUtil.none
    }
  }

  /**
    * Inserts the UTXO at the given index into our DB, swallowing the
    * error if any (this is because we're operating on data we've
    * already verified).
    */
  private def processUtxo(
      transaction: Transaction,
      index: Int,
      state: TxoState,
      blockHash: Option[DoubleSha256DigestBE]): Future[SpendingInfoDb] = {
    val addUtxoF = addUtxo(transaction = transaction,
                           vout = UInt32(index),
                           state = state,
                           blockHash = blockHash)
    addUtxoF.flatMap {
      case AddUtxoSuccess(utxo) => Future.successful(utxo)
      case err: AddUtxoError =>
        logger.error(s"Could not add UTXO", err)
        Future.failed(err)
    }
  }

  private case class OutputWithIndex(output: TransactionOutput, index: Int)

  /**
    * Processes an incoming transaction that already exists in our wallet.
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
      (foundTxo.blockHash, blockHashOpt) match {
        case (None, Some(blockHash)) =>
          logger.debug(
            s"Updating block_hash of txo=${transaction.txIdBE}, new block hash=$blockHash")
          // update block hash
          val txoWithHash = foundTxo.copyWithBlockHash(blockHash = blockHash)

          // If the utxo was marked reserved we want to update it to spent now
          // since it has been included in a block
          val unreservedTxo = txoWithHash.state match {
            case TxoState.Reserved =>
              txoWithHash.copyWithState(TxoState.PendingConfirmationsSpent)
            case TxoState.PendingConfirmationsReceived |
                TxoState.ConfirmedReceived |
                TxoState.PendingConfirmationsSpent | TxoState.ConfirmedSpent |
                TxoState.DoesNotExist =>
              txoWithHash
          }

          // Update Txo State
          val updateF = updateUtxoConfirmedState(unreservedTxo)

          updateF.foreach(tx =>
            logger.debug(
              s"Updated block_hash of txo=${tx.txid.hex} new block hash=${blockHash.hex}"))
          updateF.failed.foreach(err =>
            logger.error(
              s"Failed to update confirmation count of transaction=${transaction.txIdBE.hex}",
              err))

          updateF
        case (Some(oldBlockHash), Some(newBlockHash)) =>
          if (oldBlockHash == newBlockHash) {
            logger.debug(
              s"Skipping further processing of transaction=${transaction.txIdBE}, already processed.")

            for {
              relevantOuts <- getRelevantOutputs(transaction)
              totalIncoming = relevantOuts.map(_.output.value).sum
              _ <- insertIncomingTransaction(transaction, totalIncoming)
            } yield foundTxo
          } else {
            val errMsg =
              Seq(
                s"Found TXO has block hash=${oldBlockHash}, tx we were given has block hash=${newBlockHash}.",
                "This is either a reorg or a double spent, which is not implemented yet"
              ).mkString(" ")
            logger.error(errMsg)
            Future.failed(new RuntimeException(errMsg))
          }
        case (Some(blockHash), None) =>
          val msg =
            List(
              s"Incoming transaction=${transaction.txIdBE} already has block hash=$blockHash! assigned",
              s" I don't know how to handle this."
            ).mkString(" ")
          logger.warn(msg)
          Future.failed(new RuntimeException(msg))
        case (None, None) =>
          logger.debug(
            s"Skipping further processing of transaction=${transaction.txIdBE}, already processed.")
          Future.successful(foundTxo)
      }
    }
  }

  private def addUTXOsFut(
      outputsWithIndex: Seq[OutputWithIndex],
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[Seq[SpendingInfoDb]] =
    Future
      .sequence {
        outputsWithIndex.map(out =>
          processUtxo(
            transaction,
            out.index,
            // TODO is this correct?
            //we probably need to incorporate what
            //what our wallet's desired confirmation number is
            state = TxoState.PendingConfirmationsReceived,
            blockHash = blockHashOpt
          ))
      }

  private[wallet] def insertIncomingTransaction(
      transaction: Transaction,
      incomingAmount: CurrencyUnit): Future[IncomingTransactionDb] = {
    val txDb = TransactionDb.fromTransaction(transaction)
    val incomingDb = IncomingTransactionDb(transaction.txIdBE, incomingAmount)
    for {
      _ <- transactionDAO.upsert(txDb)
      written <- incomingTxDAO.upsert(incomingDb)
    } yield written
  }

  private def getRelevantOutputs(
      transaction: Transaction): Future[Seq[OutputWithIndex]] = {
    addressDAO.findAll().map { addrs =>
      val withIndex =
        transaction.outputs.zipWithIndex
      withIndex.collect {
        case (out, idx)
            if addrs.map(_.scriptPubKey).contains(out.scriptPubKey) =>
          OutputWithIndex(out, idx)
      }
    }
  }

  /**
    * Processes an incoming transaction that's new to us
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
          s"Found $count relevant output(s) in transaction=${transaction.txIdBE}: $outputStr")

        val totalIncoming = outputsWithIndex.map(_.output.value).sum

        for {
          _ <- insertIncomingTransaction(transaction, totalIncoming)
          prevTagDbs <- addressTagDAO.findTx(transaction, networkParameters)
          prevTags = prevTagDbs.map(_.addressTag)
          tagsToUse =
            prevTags
              .filterNot(tag => newTags.contains(tag)) ++ newTags
          newTagDbs = outputsWithIndex.flatMap { out =>
            val address = BitcoinAddress
              .fromScriptPubKey(out.output.scriptPubKey, networkParameters)
            tagsToUse.map(tag => AddressTagDb(address, tag))
          }
          _ <- addressTagDAO.createAll(newTagDbs.toVector)
          utxos <- addUTXOsFut(outputsWithIndex, transaction, blockHashOpt)
        } yield utxos
    }
  }
}
