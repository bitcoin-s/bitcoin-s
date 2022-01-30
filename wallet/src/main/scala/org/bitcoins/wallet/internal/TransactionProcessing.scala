package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.api.wallet.{AddUtxoError, AddUtxoSuccess}
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.core.wallet.utxo.{AddressTag, ReceivedState, TxoState}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import org.bitcoins.wallet._

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}

/** Provides functionality for processing transactions. This
  * includes importing UTXOs spent to our wallet, updating
  * confirmation counts and marking UTXOs as spent when
  * spending from our wallet
  */
private[bitcoins] trait TransactionProcessing extends WalletLogger {
  self: Wallet =>

  /////////////////////
  // Public facing API

  /** @inheritdoc */
  override def processTransaction(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Wallet] = {
    for {
      result <- processTransactionImpl(transaction = transaction,
                                       blockHashOpt = blockHashOpt,
                                       newTags = Vector.empty,
                                       receivedSpendingInfoDbsOpt = None,
                                       spentSpendingInfoDbsOpt = None)
    } yield {
      logger.debug(
        s"Finished processing of transaction=${transaction.txIdBE.hex}. Relevant incomingTXOs=${result.updatedIncoming.length}, outgoingTXOs=${result.updatedOutgoing.length}")
      this
    }
  }

  override def processBlock(block: Block): Future[Wallet] = {
    logger.info(s"Processing block=${block.blockHeader.hash.flip.hex}")
    val start = TimeUtil.currentEpochMs
    val isEmptyF = isEmpty()
    val heightF = chainQueryApi.getBlockHeight(block.blockHeader.hashBE)
    val resF = for {
      isEmpty <- isEmptyF
      newWallet <- {
        if (!isEmpty) {
          processBlockCachedUtxos(block)
        } else {
          //do nothing if the wallet is empty as an optimization
          //this is for users first downloading bitcoin-s
          //and syncing their node
          Future.successful(this)
        }
      }
    } yield newWallet

    val f = for {
      res <- resF
      hash = block.blockHeader.hashBE
      height <- heightF
      _ <- stateDescriptorDAO.updateSyncHeight(hash, height.get)
      _ <- walletConfig.walletCallbacks.executeOnBlockProcessed(logger, block)
    } yield {
      res
    }

    f.onComplete(failure =>
      signalBlockProcessingCompletion(block.blockHeader.hash, failure))

    f.foreach { _ =>
      val stop = TimeUtil.currentEpochMs
      logger.info(
        s"Finished processing of block=${block.blockHeader.hash.flip.hex}. It took ${stop - start}ms")
    }
    f.failed.foreach(e =>
      logger.error(
        s"Error processing of block=${block.blockHeader.hash.flip.hex}.",
        e))
    f
  }

  /** Helper method to process a block. This fetches all of our relevent spending info dbs
    * up front rather than fetching them every time [[processTransaction]] is called. This
    * significantly improves performance on rescans or IBD with an existing wallet
    */
  private def processBlockCachedUtxos(block: Block): Future[Wallet] = {
    //fetch all received spending info dbs relevant to txs in this block to improve performance
    val receivedSpendingInfoDbsF =
      spendingInfoDAO
        .findTxs(block.transactions.toVector)

    val cachedReceivedOptF = receivedSpendingInfoDbsF
      .map(Some(_)) //reduce allocations by creating Some here

    //fetch all spending infoDbs for this block to improve performance
    val spentSpendingInfoDbsF =
      spendingInfoDAO.findOutputsBeingSpent(block.transactions.toVector)

    val blockHashOpt = Some(block.blockHeader.hash.flip)
    val resultF: Future[Future[Wallet]] = for {
      //map on these first so we don't have to call
      //.map everytime we iterate through a tx
      //which is costly (thread overhead)
      receivedSpendingInfoDbsOpt <- cachedReceivedOptF
      spentSpendingInfoDbs <- spentSpendingInfoDbsF
    } yield {
      //we need to keep a cache of spentSpendingInfoDb
      //for the case where we receive & then spend that
      //same utxo in the same block
      var cachedSpentOpt: Option[Vector[SpendingInfoDb]] = {
        Some(spentSpendingInfoDbs)
      }
      val blockInputs = block.transactions.flatMap(_.inputs)
      val wallet: Future[Wallet] = {
        block.transactions.foldLeft(Future.successful(this)) {
          (walletF, transaction) =>
            for {
              wallet <- walletF
              processTxResult <- {
                wallet.processTransactionImpl(
                  transaction = transaction,
                  blockHashOpt = blockHashOpt,
                  newTags = Vector.empty,
                  receivedSpendingInfoDbsOpt = receivedSpendingInfoDbsOpt,
                  spentSpendingInfoDbsOpt = cachedSpentOpt
                )
              }
              _ = {
                //need to look if a received utxo is spent in the same block
                //if so, we need to update our cachedSpentF
                val spentInSameBlock: Vector[SpendingInfoDb] = {
                  processTxResult.updatedIncoming.filter { spendingInfoDb =>
                    blockInputs.exists(
                      _.previousOutput == spendingInfoDb.outPoint)
                  }
                }

                //add it to the cache
                val newCachedSpentOpt = {
                  cachedSpentOpt match {
                    case Some(spentSpendingInfo) =>
                      Some(spentSpendingInfo ++ spentInSameBlock)
                    case None =>
                      Some(spentInSameBlock)
                  }
                }
                cachedSpentOpt = newCachedSpentOpt
              }
            } yield {
              this
            }
        }
      }
      wallet
    }

    resultF.flatten
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
      s"Processing TX from our wallet, transaction=${transaction.txIdBE.hex} with blockHash=${blockHashOpt
        .map(_.hex)}")
    for {
      (txDb, _) <-
        insertOutgoingTransaction(transaction,
                                  feeRate,
                                  inputAmount,
                                  sentAmount,
                                  blockHashOpt)
      result <- processTransactionImpl(transaction = txDb.transaction,
                                       blockHashOpt = blockHashOpt,
                                       newTags = newTags,
                                       receivedSpendingInfoDbsOpt = None,
                                       spentSpendingInfoDbsOpt = None)
    } yield {
      val txid = txDb.transaction.txIdBE
      val changeOutputs = result.updatedIncoming.length
      val spentOutputs = result.updatedOutgoing.length

      logger.info(
        s"Processing of internal transaction=${txid.hex} resulted in changeOutputs=$changeOutputs and spentUTXOs=$spentOutputs")
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
      logger.debug(
        s"Updating wallet signal completion for ${blockHash.flip.hex}")
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

  /** Processes received utxos that are contained in the given transaction
    * @param transaction the transaction that we are receiving utxos from
    * @param blockHashOpt the block hash that contains this tx
    * @param spendingInfoDbs the spending info dbs that are relevant for this transaction
    * @param newTags tags associated with this tx
    * @return
    */
  protected def processReceivedUtxos(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      spendingInfoDbs: Vector[SpendingInfoDb],
      newTags: Vector[AddressTag]): Future[Vector[SpendingInfoDb]] = {
    if (spendingInfoDbs.isEmpty) {
      processNewReceivedTx(transaction, blockHashOpt, newTags)
        .map(_.toVector)
    } else {
      val processedVec = spendingInfoDbs.map { txo =>
        processExistingReceivedTxo(transaction, blockHashOpt, txo)
      }
      Future.sequence(processedVec)
    }
  }

  /** Searches for outputs on the given transaction that are
    * being spent from our wallet
    */
  protected def processSpentUtxos(
      transaction: Transaction,
      outputsBeingSpent: Vector[SpendingInfoDb],
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[
    Vector[SpendingInfoDb]] = {
    for {
      _ <- {
        if (outputsBeingSpent.nonEmpty)
          insertTransaction(transaction, blockHashOpt)
        else Future.unit
      }
      toBeUpdated = outputsBeingSpent
        .map(markAsSpent(_, transaction.txIdBE))
        .flatten
      processed <- spendingInfoDAO.updateAllSpendingInfoDb(toBeUpdated)
      _ <- updateUtxoConfirmedStates(processed)
    } yield {
      processed
    }
  }

  /** Does the grunt work of processing a TX.
    * This is called by either the internal or public TX
    * processing method, which logs and transforms the
    * output fittingly.
    */
  private[internal] def processTransactionImpl(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag],
      receivedSpendingInfoDbsOpt: Option[Vector[SpendingInfoDb]],
      spentSpendingInfoDbsOpt: Option[Vector[SpendingInfoDb]]): Future[
    ProcessTxResult] = {

    logger.debug(
      s"Processing transaction=${transaction.txIdBE.hex} with blockHash=${blockHashOpt
        .map(_.hex)}")

    val receivedSpendingInfoDbsF: Future[Vector[SpendingInfoDb]] = {
      receivedSpendingInfoDbsOpt match {
        case Some(received) =>
          //spending info dbs are cached, so fetch the one relevant for this tx
          val filtered = received.filter(_.txid == transaction.txIdBE)
          Future.successful(filtered)
        case None =>
          //no caching, just fetch from the database
          spendingInfoDAO.findTx(transaction)
      }

    }
    val spentSpendingInfoDbsF: Future[Vector[SpendingInfoDb]] = {
      spentSpendingInfoDbsOpt match {
        case Some(spent) =>
          //spending info dbs are cached, so filter for outpoints related to this tx
          val filtered = spent.filter { s =>
            transaction.inputs.exists(_.previousOutput == s.outPoint)
          }
          Future.successful(filtered)
        case None =>
          //no caching, just fetch from db
          spendingInfoDAO.findOutputsBeingSpent(transaction)
      }
    }

    for {
      receivedSpendingInfoDbs <- receivedSpendingInfoDbsF
      receivedStart = TimeUtil.currentEpochMs
      incoming <- processReceivedUtxos(transaction = transaction,
                                       blockHashOpt = blockHashOpt,
                                       spendingInfoDbs =
                                         receivedSpendingInfoDbs,
                                       newTags = newTags)
      _ = if (incoming.nonEmpty) {
        logger.info(
          s"Finished processing ${incoming.length} received outputs, it took=${TimeUtil.currentEpochMs - receivedStart}ms")
      }

      spentSpendingInfoDbs <- spentSpendingInfoDbsF
      spentStart = TimeUtil.currentEpochMs
      outgoing <- processSpentUtxos(transaction = transaction,
                                    outputsBeingSpent = spentSpendingInfoDbs,
                                    blockHashOpt = blockHashOpt)
      _ = if (outgoing.nonEmpty) {
        logger.info(
          s"Finished processing ${outgoing.length} spent outputs, it took=${TimeUtil.currentEpochMs - spentStart}ms")
      }
      _ <-
        // only notify about our transactions
        if (incoming.nonEmpty || outgoing.nonEmpty)
          walletCallbacks.executeOnTransactionProcessed(logger, transaction)
        else Future.unit
    } yield {
      ProcessTxResult(incoming, outgoing)
    }
  }

  /** If the given UTXO is marked as unspent and returns it so it can be updated
    * Otherwise returns None.
    *
    * If the utxo is transitioning into an invalid state it throws ane exception.
    */
  private def markAsSpent(
      out: SpendingInfoDb,
      spendingTxId: DoubleSha256DigestBE): Option[SpendingInfoDb] = {
    out.state match {
      case ConfirmedReceived | PendingConfirmationsReceived |
          BroadcastReceived =>
        val updated =
          out
            .copyWithSpendingTxId(spendingTxId)
            .copyWithState(state = BroadcastSpent)
        logger.debug(
          s"Marked utxo=${updated.toHumanReadableString} as state=${updated.state}")
        Some(updated)
      case TxoState.Reserved =>
        val updated =
          out
            .copyWithSpendingTxId(spendingTxId)
            .copyWithState(state = BroadcastSpent)
        Some(updated)
      case TxoState.BroadcastSpent =>
        if (!out.spendingTxIdOpt.contains(spendingTxId)) {
          logger.warn(
            s"Updating the spendingTxId of a transaction that is already spent, " +
              s"old state=${TxoState.BroadcastSpent} old spendingTxId=${out.spendingTxIdOpt
                .map(_.hex)} new spendingTxId=${spendingTxId.hex}")
        }
        val updated =
          out.copyWithSpendingTxId(spendingTxId)
        Some(updated)
      case TxoState.ImmatureCoinbase =>
        throw new RuntimeException(
          s"Attempting to spend an ImmatureCoinbase ${out.outPoint.hex}, this should not be possible until it is confirmed.")
      case TxoState.ConfirmedSpent | TxoState.PendingConfirmationsSpent =>
        if (!out.spendingTxIdOpt.contains(spendingTxId)) {
          throw new RuntimeException(
            s"Attempted to mark an already spent utxo ${out.outPoint.hex} with a new spending tx ${spendingTxId.hex}")
        } else {
          //do not want to update again
          None
        }
      case TxoState.DoesNotExist =>
        throw new RuntimeException(
          s"Attempted to process a transaction for a utxo that does not exist ${out.outPoint.hex} with a new spending tx ${spendingTxId.hex}")
    }
  }

  /** Inserts the UTXO at the given index into our DB, swallowing the
    * error if any (this is because we're operating on data we've
    * already verified).
    */
  private def processReceivedUtxo(
      transaction: Transaction,
      index: Int,
      state: ReceivedState,
      addressDbE: Either[AddUtxoError, AddressDb]): Future[SpendingInfoDb] = {
    val addIncomingUtxoF =
      addUtxo(transaction = transaction,
              vout = UInt32(index),
              state = state,
              addressDbE = addressDbE)
    addIncomingUtxoF.flatMap {
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
  private def processExistingReceivedTxo(
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
            s"Updating block_hash of txo=${transaction.txIdBE.hex}, new block hash=${blockHash.hex}")

          val updateTxDbF = insertTransaction(transaction, blockHashOpt)

          // Update Txo State
          updateTxDbF.flatMap(_ =>
            updateUtxoConfirmedState(foundTxo).flatMap {
              case Some(txo) =>
                logger.debug(
                  s"Updated block_hash of txo=${txo.txid.hex} new block hash=${blockHash.hex}")
                Future.successful(txo)
              case None =>
                // State was not updated so we need to update it so it's block hash is in the database
                spendingInfoDAO.update(foundTxo)
            })
        case None =>
          logger.debug(
            s"Skipping further processing of transaction=${transaction.txIdBE.hex}, already processed.")
          Future.successful(foundTxo)
      }
    }
  }

  /** Adds utxos to the database that we are receiving */
  private def addReceivedUTXOs(
      outputsWithIndex: Seq[OutputWithIndex],
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]): Future[
    Seq[SpendingInfoDb]] = {
    val stateF: Future[ReceivedState] = blockHashOpt match {
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

    val addressDbsF: Future[Vector[AddressDb]] = {
      getAddressDbs(outputsWithIndex.map(_.output.scriptPubKey).toVector)
    }

    val addressDbWithOutputF = for {
      addressDbs <- addressDbsF
    } yield matchAddressDbWithOutputs(addressDbs, outputsWithIndex.toVector)

    val nested = for {
      state <- stateF
      addressDbWithOutput <- addressDbWithOutputF
    } yield {
      val outputsVec = addressDbWithOutput.map { case (addressDb, out) =>
        require(addressDb.scriptPubKey == out.output.scriptPubKey)
        processReceivedUtxo(
          transaction,
          out.index,
          state = state,
          Right(addressDb)
        )
      }
      Future.sequence(outputsVec)
    }

    nested.flatten
  }

  /** Tries to convert the provided spk to an address, and then checks if we have
    * it in our address table
    */
  private def getAddressDbs(
      spks: Vector[ScriptPubKey]): Future[Vector[AddressDb]] = {
    val addressDbF: Future[Vector[AddressDb]] =
      addressDAO.findByScriptPubKeys(spks)
    addressDbF
  }

  /** Matches address dbs with outputs, drops addressDb/outputs that do not have matches */
  private def matchAddressDbWithOutputs(
      addressDbs: Vector[AddressDb],
      outputsWithIndex: Vector[OutputWithIndex]): Vector[
    (AddressDb, OutputWithIndex)] = {

    val addressDbsWithOutputsOpt = outputsWithIndex.map { out =>
      //find address associated with spk
      val addressDbOpt =
        addressDbs.find(_.scriptPubKey == out.output.scriptPubKey)
      addressDbOpt match {
        case None =>
          logger.warn(s"Could not find address associated with output=${out}")
          None
        case Some(addressDb) =>
          Some((addressDb, out))
      }
    }
    //get rid of outputs we couldn't match to an address
    addressDbsWithOutputsOpt.flatten
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
      transaction: Transaction): Future[Vector[OutputWithIndex]] = {
    val spks = transaction.outputs.map(_.scriptPubKey)
    scriptPubKeyDAO.findScriptPubKeys(spks.toVector).map { addrs =>
      val withIndex =
        transaction.outputs.zipWithIndex
      withIndex.collect {
        case (out, idx)
            if addrs.map(_.scriptPubKey).contains(out.scriptPubKey) =>
          OutputWithIndex(out, idx)
      }.toVector
    }
  }

  /** Processes an incoming transaction that's new to us
    *
    * @return A list of inserted transaction outputs
    */
  private def processNewReceivedTx(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag]): Future[Seq[SpendingInfoDb]] = {
    val outputsF = getRelevantOutputs(transaction)
    outputsF.flatMap {
      case Vector() =>
        logger.trace(
          s"Found no outputs relevant to us in transaction${transaction.txIdBE.hex}")
        Future.successful(Vector.empty)

      case outputsWithIndex =>
        val totalIncoming = outputsWithIndex.map(_.output.value).sum

        val spks = outputsWithIndex.map(_.output.scriptPubKey)
        val spksInDbF = addressDAO.findByScriptPubKeys(spks)

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
          created <- addressTagDAO.createAll(newTagDbs)
        } yield created

        for {
          (txDb, _) <- txDbF
          ourOutputs <- ourOutputsF
          utxos <- addReceivedUTXOs(ourOutputs, txDb.transaction, blockHashOpt)
          _ <- newTagsF
        } yield utxos
    }
  }

  private[wallet] def getTransactionsToBroadcast: Future[
    Vector[Transaction]] = {
    for {
      mempoolUtxos <- spendingInfoDAO.findAllInMempool
      txIds = mempoolUtxos.map { utxo =>
        utxo.spendingTxIdOpt.getOrElse(utxo.txid)
      }
      txDbs <- transactionDAO.findByTxIdBEs(txIds)
    } yield txDbs.map(_.transaction)
  }
}
