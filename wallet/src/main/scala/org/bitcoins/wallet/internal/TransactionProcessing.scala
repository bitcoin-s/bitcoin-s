package org.bitcoins.wallet.internal

import org.bitcoins.core.api.chain.ChainQueryApi
import org.bitcoins.core.api.wallet.{
  ProcessTxResult,
  TransactionProcessingApi,
  WalletApi
}
import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  OutputWithIndex,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.TxoState.*
import org.bitcoins.core.wallet.utxo.{AddressTag, TxoState}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.SafeDatabase
import org.bitcoins.wallet.*
import org.bitcoins.wallet.callback.WalletCallbacks
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AddressDAO,
  AddressTagDAO,
  IncomingTransactionDAO,
  OutgoingTransactionDAO,
  ScriptPubKeyDAO,
  SpendingInfoDAO,
  TransactionDAO,
  WalletDAOs,
  WalletStateDescriptorDAO
}
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

/** Provides functionality for processing transactions. This includes importing
  * UTXOs spent to our wallet, updating confirmation counts and marking UTXOs as
  * spent when spending from our wallet
  */
case class TransactionProcessing(
    walletApi: WalletApi,
    chainQueryApi: ChainQueryApi,
    utxoHandling: UtxoHandling,
    walletDAOs: WalletDAOs)(implicit
    walletConfig: WalletAppConfig,
    ec: ExecutionContext)
    extends TransactionProcessingApi
    with WalletLogger {
  import org.bitcoins.core.currency.currencyUnitNumeric
  private def walletCallbacks: WalletCallbacks = walletConfig.callBacks
  private val stateDescriptorDAO: WalletStateDescriptorDAO =
    walletDAOs.stateDescriptorDAO
  private val spendingInfoDAO: SpendingInfoDAO = walletDAOs.utxoDAO
  private val transactionDAO: TransactionDAO = walletDAOs.transactionDAO
  private val scriptPubKeyDAO: ScriptPubKeyDAO = walletDAOs.scriptPubKeyDAO
  private val addressDAO: AddressDAO = walletDAOs.addressDAO
  private val incomingTxDAO: IncomingTransactionDAO = walletDAOs.incomingTxDAO
  private val outgoingTxDAO: OutgoingTransactionDAO = walletDAOs.outgoingTxDAO
  private val addressTagDAO: AddressTagDAO = walletDAOs.addressTagDAO
  private val safeDatabase: SafeDatabase = stateDescriptorDAO.safeDatabase
  private val networkParameters: NetworkParameters = walletConfig.network

  /////////////////////
  // Public facing API
  override def findByTxIds(
      txIds: Vector[DoubleSha256DigestBE]
  ): Future[Vector[TransactionDb]] = {
    transactionDAO.findByTxIds(txIds)
  }

  /** @inheritdoc */
  override def processTransaction(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Unit] = {
    for {
      relevantReceivedOutputs <- getRelevantOutputs(transaction)
      result <- processTransactionImpl(
        transaction = transaction,
        blockHashOpt = blockHashOpt,
        newTags = Vector.empty,
        receivedSpendingInfoDbsOpt = None,
        spentSpendingInfoDbsOpt = None,
        relevantReceivedOutputs = relevantReceivedOutputs
      )
    } yield {
      if (result.updatedIncoming.nonEmpty || result.updatedOutgoing.nonEmpty) {
        logger.info(
          s"Finished processing of transaction=${transaction.txIdBE.hex}. Relevant incomingTXOs=${result.updatedIncoming.length}, outgoingTXOs=${result.updatedOutgoing.length}"
        )

      }
      ()
    }
  }

  override def processBlock(block: Block): Future[Unit] = {
    val start = TimeUtil.currentEpochMs
    val isEmptyF = walletApi.isEmpty()
    val heightF = chainQueryApi.getBlockHeight(block.blockHeader.hashBE)
    heightF.foreach { heightOpt =>
      logger.debug(
        s"Processing block=${block.blockHeader.hashBE.hex} heightOpt=$heightOpt"
      )
    }

    heightF.flatMap {
      case Some(height) =>
        val resF = for {
          isEmpty <- isEmptyF
          _ <- {
            if (!isEmpty) {
              processBlockCachedUtxos(block)
            } else {
              // do nothing if the wallet is empty as an optimization
              // this is for users first downloading bitcoin-s
              // and syncing their node
              Future.successful(this)
            }
          }
        } yield ()

        val f = for {
          _ <- resF
          hash = block.blockHeader.hashBE
          _ <- stateDescriptorDAO.updateSyncHeight(hash, height)
          _ <- walletConfig.callBacks.executeOnBlockProcessed(block)
        } yield {
          ()
        }

        f.onComplete(failure =>
          signalBlockProcessingCompletion(block.blockHeader.hashBE, failure))

        f.foreach { _ =>
          val stop = TimeUtil.currentEpochMs
          logger.info(
            s"Finished processing of block=${block.blockHeader.hash.flip.hex}. It took ${stop - start}ms"
          )
        }
        f.failed.foreach(e =>
          logger.error(
            s"Error processing of block=${block.blockHeader.hash.flip.hex}.",
            e
          ))
        f
      case None =>
        logger.warn(
          s"Could not find blockheight for blockHash=${block.blockHeader.hashBE.hex}, skipping processing in wallet"
        )
        Future.unit
    }

  }

  /** Helper method to process a block. This fetches all of our relevent
    * spending info dbs up front rather than fetching them every time
    * [[processTransaction]] is called. This significantly improves performance
    * on rescans or IBD with an existing wallet
    */
  private def processBlockCachedUtxos(block: Block): Future[Unit] = {
    // fetch all received spending info dbs relevant to txs in this block to improve performance
    val receivedSpendingInfoDbsF =
      spendingInfoDAO
        .findTxs(block.transactions.toVector)

    val cachedReceivedOptF = receivedSpendingInfoDbsF
      .map(Some(_)) // reduce allocations by creating Some here

    // fetch all spending infoDbs for this block to improve performance
    val spentSpendingInfoDbsF =
      spendingInfoDAO.findOutputsBeingSpent(block.transactions.toVector)

    val blockHashOpt = Some(block.blockHeader.hash.flip)

    // fetch all outputs we may have received in this block in advance
    // as an optimization
    val relevantReceivedOutputsForBlockF = getRelevantOutputsForBlock(block)

    val resultF: Future[Future[Unit]] = for {
      // map on these first so we don't have to call
      // .map everytime we iterate through a tx
      // which is costly (thread overhead)
      receivedSpendingInfoDbsOpt <- cachedReceivedOptF
      spentSpendingInfoDbs <- spentSpendingInfoDbsF
      relevantReceivedOutputsForBlock <-
        relevantReceivedOutputsForBlockF
    } yield {
      // we need to keep a cache of spentSpendingInfoDb
      // for the case where we receive & then spend that
      // same utxo in the same block
      var cachedSpentOpt: Option[Vector[SpendingInfoDb]] = {
        Some(spentSpendingInfoDbs)
      }
      val blockInputs = block.transactions.flatMap(_.inputs)
      val processedF: Future[Unit] = {
        block.transactions.foldLeft(Future.successful(())) {
          (walletF, transaction) =>
            for {
              _ <- walletF
              relevantReceivedOutputsForTx = relevantReceivedOutputsForBlock
                .getOrElse(transaction.txIdBE, Vector.empty)
              processTxResult <- {
                processTransactionImpl(
                  transaction = transaction,
                  blockHashOpt = blockHashOpt,
                  newTags = Vector.empty,
                  receivedSpendingInfoDbsOpt = receivedSpendingInfoDbsOpt,
                  spentSpendingInfoDbsOpt = cachedSpentOpt,
                  relevantReceivedOutputs = relevantReceivedOutputsForTx
                )
              }
              _ = {
                // need to look if a received utxo is spent in the same block
                // if so, we need to update our cachedSpentF
                val spentInSameBlock: Vector[SpendingInfoDb] = {
                  processTxResult.updatedIncoming.filter { spendingInfoDb =>
                    blockInputs.exists(
                      _.previousOutput == spendingInfoDb.outPoint
                    )
                  }
                }

                // add it to the cache
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
              ()
            }
        }
      }
      processedF
    }

    resultF.flatten
  }

  override def findTransaction(
      txId: DoubleSha256DigestBE
  ): Future[Option[TransactionDb]] = {
    transactionDAO.findByTxId(txId)
  }

  override def listTransactions(): Future[Vector[TransactionDb]] =
    transactionDAO.findAll()

  /////////////////////
  // Internal wallet API

  override def insertTransaction(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[TransactionDb] = {
    safeDatabase.run(insertTransactionAction(tx, blockHashOpt))
  }

  protected def insertTransactionAction(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): DBIOAction[TransactionDb, NoStream, Effect.Write with Effect.Read] = {
    val txDb = TransactionDbHelper.fromTransaction(tx, blockHashOpt)
    transactionDAO.upsertAction(txDb)
  }

  private[wallet] def insertOutgoingTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[(TransactionDb, OutgoingTransactionDb)] = {
    val outgoingDb =
      OutgoingTransactionDb.fromTransaction(
        transaction,
        inputAmount,
        sentAmount,
        feeRate.calc(transaction)
      )
    for {
      txDb <- insertTransaction(transaction, blockHashOpt)
      written <- outgoingTxDAO.upsert(outgoingDb)
    } yield (txDb, written)
  }

  /** Processes TXs originating from our wallet. This is called right after
    * we've signed a TX, updating our UTXO state.
    */
  override def processOurTransaction(
      transaction: Transaction,
      feeRate: FeeUnit,
      inputAmount: CurrencyUnit,
      sentAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag]
  ): Future[ProcessTxResult] = {
    logger.info(
      s"Processing TX from our wallet, transaction=${transaction.txIdBE.hex} with blockHash=${blockHashOpt
          .map(_.hex)}"
    )
    val relevantOutputsF = getRelevantOutputs(transaction)
    for {
      (txDb, _) <-
        insertOutgoingTransaction(
          transaction,
          feeRate,
          inputAmount,
          sentAmount,
          blockHashOpt
        )
      relevantOutputs <- relevantOutputsF
      result <- processTransactionImpl(
        transaction = txDb.transaction,
        blockHashOpt = blockHashOpt,
        newTags = newTags,
        receivedSpendingInfoDbsOpt = None,
        spentSpendingInfoDbsOpt = None,
        relevantOutputs
      )
    } yield {
      val txid = txDb.transaction.txIdBE
      val changeOutputs = result.updatedIncoming.length
      val spentOutputs = result.updatedOutgoing.length

      logger.info(
        s"Processing of internal transaction=${txid.hex} resulted in changeOutputs=$changeOutputs and spentUTXOs=$spentOutputs"
      )
      result
    }
  }

  /////////////////////
  // Private methods

  private val blockProcessingSignals =
    mutable.Map.empty[DoubleSha256DigestBE, Promise[DoubleSha256DigestBE]]

  override def subscribeForBlockProcessingCompletionSignal(
      blockHash: DoubleSha256DigestBE
  ): Future[DoubleSha256DigestBE] =
    synchronized {
      blockProcessingSignals.get(blockHash) match {
        case Some(existingSignal) => existingSignal.future
        case None =>
          val newSignal = Promise[DoubleSha256DigestBE]()
          blockProcessingSignals.addOne((blockHash, newSignal))
          newSignal.future
      }
    }

  private def signalBlockProcessingCompletion(
      blockHash: DoubleSha256DigestBE,
      failure: Try[?]
  ): Unit = {
    synchronized {
      blockProcessingSignals.remove(blockHash) match {
        case Some(signal) =>
          failure match {
            case Success(_) =>
              signal.success(blockHash)
            case Failure(exception) => signal.failure(exception)
          }
          ()
        case None => ()
      }
    }
  }

  /** Processes received utxos that are contained in the given transaction
    * @param transaction
    *   the transaction that we are receiving utxos from
    * @param blockHashOpt
    *   the block hash that contains this tx
    * @param spendingInfoDbs
    *   the spending info dbs that are relevant for this transaction
    * @param newTags
    *   tags associated with this tx
    * @return
    */
  override def processReceivedUtxos(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      spendingInfoDbs: Vector[SpendingInfoDb],
      newTags: Vector[AddressTag],
      relevantReceivedOutputs: Vector[OutputWithIndex]
  ): Future[Vector[SpendingInfoDb]] = {
    if (spendingInfoDbs.isEmpty && relevantReceivedOutputs.isEmpty) {
      // as an optimization if we don't have any relevant utxos
      // and any relevant outputs that match scripts in our wallet
      // we can just return now
      Future.successful(Vector.empty)
    } else {
      val newOutputs = relevantReceivedOutputs.filterNot { output =>
        val outPoint =
          TransactionOutPoint(transaction.txId, UInt32(output.index))
        spendingInfoDbs.exists(_.outPoint == outPoint)
      }
      if (newOutputs.nonEmpty) {
        processNewReceivedTx(transaction, blockHashOpt, newTags, newOutputs)
          .map(_.toVector)
      } else {
        val processedVec = spendingInfoDbs.map { txo =>
          processExistingReceivedTxo(transaction, blockHashOpt, txo)
        }
        Future.sequence(processedVec)
      }
    }
  }

  /** Searches for outputs on the given transaction that are being spent from
    * our wallet
    */
  override def processSpentUtxos(
      transaction: Transaction,
      outputsBeingSpent: Vector[SpendingInfoDb],
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Vector[SpendingInfoDb]] = {
    for {
      _ <- {
        if (outputsBeingSpent.nonEmpty)
          insertTransaction(transaction, blockHashOpt)
        else Future.unit
      }
      toBeUpdated = outputsBeingSpent.flatMap(
        markAsSpent(_, transaction.txIdBE)
      )
      withBlocks <- utxoHandling.getDbsByRelevantBlock(toBeUpdated)
      processed <- utxoHandling.updateUtxoSpentConfirmedStates(withBlocks)
    } yield {
      processed
    }
  }

  /** Does the grunt work of processing a TX. This is called by either the
    * internal or public TX processing method, which logs and transforms the
    * output fittingly.
    */
  private[internal] def processTransactionImpl(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag],
      receivedSpendingInfoDbsOpt: Option[Vector[SpendingInfoDb]],
      spentSpendingInfoDbsOpt: Option[Vector[SpendingInfoDb]],
      relevantReceivedOutputs: Vector[OutputWithIndex]
  ): Future[ProcessTxResult] = {

    logger.debug(
      s"Processing transaction=${transaction.txIdBE.hex} with blockHash=${blockHashOpt
          .map(_.hex)}"
    )
    val receivedSpendingInfoDbsF: Future[Vector[SpendingInfoDb]] = {
      receivedSpendingInfoDbsOpt match {
        case Some(received) =>
          // spending info dbs are cached, so fetch the one relevant for this tx
          val filtered = received.filter(_.txid == transaction.txIdBE)
          Future.successful(filtered)
        case None =>
          // no caching, just fetch from the database
          spendingInfoDAO.findTx(transaction)
      }

    }

    val spentSpendingInfoDbsF: Future[Vector[SpendingInfoDb]] = {
      spentSpendingInfoDbsOpt match {
        case Some(spent) =>
          // spending info dbs are cached, so filter for outpoints related to this tx
          val filtered = spent.filter { s =>
            transaction.inputs.exists(_.previousOutput == s.outPoint)
          }
          Future.successful(filtered)
        case None =>
          // no caching, just fetch from db
          spendingInfoDAO.findOutputsBeingSpent(transaction)
      }
    }

    val processTxF = for {
      receivedSpendingInfoDbs <- receivedSpendingInfoDbsF
      receivedStart = TimeUtil.currentEpochMs
      incoming <- processReceivedUtxos(
        transaction = transaction,
        blockHashOpt = blockHashOpt,
        spendingInfoDbs = receivedSpendingInfoDbs,
        newTags = newTags,
        relevantReceivedOutputs = relevantReceivedOutputs
      )
      _ = if (incoming.nonEmpty) {
        logger.info(
          s"Finished processing ${incoming.length} received outputs, balance=${incoming
              .map(_.output.value)
              .sum} it took=${TimeUtil.currentEpochMs - receivedStart}ms"
        )
      }

      spentSpendingInfoDbs <- spentSpendingInfoDbsF
      spentStart = TimeUtil.currentEpochMs
      outgoing <- processSpentUtxos(
        transaction = transaction,
        outputsBeingSpent = spentSpendingInfoDbs,
        blockHashOpt = blockHashOpt
      )
      _ = if (outgoing.nonEmpty) {
        logger.info(
          s"Finished processing ${outgoing.length} spent outputs, it took=${TimeUtil.currentEpochMs - spentStart}ms"
        )
      }
      _ <-
        // only notify about our transactions
        if (incoming.nonEmpty || outgoing.nonEmpty) {
          walletCallbacks.executeOnTransactionProcessed(transaction)
        } else Future.unit
    } yield {
      ProcessTxResult(incoming, outgoing)
    }

    processTxF.failed.foreach { case err =>
      logger.error(
        s"Failed to process transaction=${transaction.txIdBE.hex} blockHash=${blockHashOpt}",
        err
      )
    }
    processTxF
  }

  /** If the given UTXO is marked as unspent and returns it so it can be updated
    * Otherwise returns None.
    *
    * If the utxo is transitioning into an invalid state it throws ane
    * exception.
    */
  private def markAsSpent(
      out: SpendingInfoDb,
      spendingTxId: DoubleSha256DigestBE
  ): Option[SpendingInfoDb] = {
    out.state match {
      case ConfirmedReceived | PendingConfirmationsReceived |
          BroadcastReceived =>
        val updated =
          out
            .copyWithSpendingTxId(spendingTxId)
            .copyWithState(state = BroadcastSpent)
        logger.debug(
          s"Marked utxo=${updated.toHumanReadableString} as state=${updated.state}"
        )
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
                  .map(_.hex)} new spendingTxId=${spendingTxId.hex}"
          )
        }
        val updated =
          out.copyWithSpendingTxId(spendingTxId)
        Some(updated)
      case TxoState.ImmatureCoinbase =>
        throw new RuntimeException(
          s"Attempting to spend an ImmatureCoinbase ${out.outPoint.hex}, this should not be possible until it is confirmed."
        )
      case TxoState.ConfirmedSpent | TxoState.PendingConfirmationsSpent =>
        if (!out.spendingTxIdOpt.contains(spendingTxId)) {
          throw new RuntimeException(
            s"Attempted to mark an already spent utxo ${out.outPoint.hex} with a new spending tx ${spendingTxId.hex}"
          )
        } else {
          // do not want to update again
          None
        }
    }
  }

  /** Processes an incoming transaction that already exists in our wallet. If
    * the incoming transaction has more confirmations than what we have in the
    * DB, we update the TX
    */
  private def processExistingReceivedTxo(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      foundTxo: SpendingInfoDb
  ): Future[SpendingInfoDb] = {
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
            s"Updating block_hash of txo=${transaction.txIdBE.hex}, new block hash=${blockHash.hex}"
          )

          val updateTxDbF = insertTransaction(transaction, blockHashOpt)
          val withBlocksF = updateTxDbF.flatMap(_ =>
            utxoHandling.getDbsByRelevantBlock(Vector(foundTxo)))
          // Update Txo State
          withBlocksF.flatMap { withBlocks =>
            utxoHandling
              .updateUtxoReceiveConfirmedStates(withBlocks)
              .flatMap { txos =>
                if (txos.length == 1) {
                  val txo = txos.head
                  logger.debug(
                    s"Updated block_hash of txo=${txo.txid.hex} new block hash=${blockHash.hex}"
                  )
                  Future.successful(txo)
                } else {
                  // State was not updated so we need to update it so it's block hash is in the database
                  spendingInfoDAO.update(foundTxo)
                }
              }
          }
        case None =>
          logger.debug(
            s"Skipping further processing of transaction=${transaction.txIdBE.hex}, already processed."
          )
          Future.successful(foundTxo)
      }
    }
  }

  /** Adds utxos to the database that we are receiving */
  private def addReceivedUTXOs(
      outputsWithIndex: Seq[OutputWithIndex],
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): Future[Seq[SpendingInfoDb]] = {

    val spks = outputsWithIndex.map(_.output.scriptPubKey).toVector

    val addressDbWithOutputA = for {
      addressDbs <- addressDAO.findByScriptPubKeysAction(spks)
    } yield {
      if (addressDbs.isEmpty) {
        logger.warn(
          s"Found zero addresses in the database to match an output we have a script for, txid=${transaction.txIdBE.hex} outputs=$outputsWithIndex"
        )
      }
      matchAddressDbWithOutputs(addressDbs, outputsWithIndex.toVector)
    }

    val nested = for {
      addressDbWithOutput <- safeDatabase.run(addressDbWithOutputA)
    } yield {
      val outputsVec = addressDbWithOutput.map { case (addressDb, out) =>
        require(addressDb.scriptPubKey == out.output.scriptPubKey)
        utxoHandling.processReceivedUtxo(
          transaction,
          out.index,
          blockHashOpt,
          addressDb
        )
      }
      Future.sequence(outputsVec)
    }

    nested.flatten
  }

  /** Matches address dbs with outputs, drops addressDb/outputs that do not have
    * matches
    */
  private def matchAddressDbWithOutputs(
      addressDbs: Vector[AddressDb],
      outputsWithIndex: Vector[OutputWithIndex]
  ): Vector[(AddressDb, OutputWithIndex)] = {
    val addressDbsWithOutputsOpt = outputsWithIndex.map { out =>
      // find address associated with spk
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
    // get rid of outputs we couldn't match to an address
    val result = addressDbsWithOutputsOpt.flatten
    result
  }

  private[wallet] def insertIncomingTransaction(
      transaction: Transaction,
      incomingAmount: CurrencyUnit,
      blockHashOpt: Option[DoubleSha256DigestBE]
  ): DBIOAction[
    (TransactionDb, IncomingTransactionDb),
    NoStream,
    Effect.Read with Effect.Write
  ] = {
    val incomingDb = IncomingTransactionDb(transaction.txIdBE, incomingAmount)
    for {
      txDb <- insertTransactionAction(transaction, blockHashOpt)
      written <- incomingTxDAO.upsertAction(incomingDb)
    } yield (txDb, written)
  }

  /** Filters outputs on tx so that only relevant outputs to our wallet are
    * included
    */
  private def getRelevantOutputs(
      transaction: Transaction
  ): Future[Vector[OutputWithIndex]] = {
    val spks = transaction.outputs.map(_.scriptPubKey)
    scriptPubKeyDAO.findScriptPubKeys(spks.toVector).map { addrs =>
      matchReceivedTx(addrs, transaction)
    }
  }

  private def matchReceivedTx(
      addrs: Vector[ScriptPubKeyDb],
      transaction: Transaction
  ): Vector[OutputWithIndex] = {
    val withIndex =
      transaction.outputs.zipWithIndex
    withIndex.collect {
      case (out, idx) if addrs.map(_.scriptPubKey).contains(out.scriptPubKey) =>
        OutputWithIndex(out, idx)
    }.toVector
  }

  private def getRelevantOutputsForBlock(
      block: Block
  ): Future[Map[DoubleSha256DigestBE, Vector[OutputWithIndex]]] = {
    val spksInBlock: Vector[ScriptPubKey] = block.transactions
      .flatMap(tx => tx.outputs.map(o => o.scriptPubKey))
      .toVector
    val spksInDbF = scriptPubKeyDAO.findScriptPubKeys(spksInBlock)

    spksInDbF.map { addrs =>
      block.transactions.foldLeft(
        Map.empty[DoubleSha256DigestBE, Vector[OutputWithIndex]]
      ) { (acc, tx) =>
        acc.updated(tx.txIdBE, matchReceivedTx(addrs, tx))
      }
    }
  }

  /** Processes an incoming transaction that's new to us
    *
    * @return
    *   A list of inserted transaction outputs
    */
  private def processNewReceivedTx(
      transaction: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      newTags: Vector[AddressTag],
      relevantReceivedOutputs: Vector[OutputWithIndex]
  ): Future[Seq[SpendingInfoDb]] = {
    if (relevantReceivedOutputs.isEmpty) {

      logger.trace(
        s"Found no outputs relevant to us in transaction${transaction.txIdBE.hex}"
      )

      Future.successful(Vector.empty)
    } else {
      val filteredOutputs =
        transaction.outputs.zipWithIndex.filter(o =>
          relevantReceivedOutputs.contains(OutputWithIndex(o._1, o._2)))

      if (filteredOutputs.isEmpty) {
        // no relevant outputs in this tx, return early
        Future.successful(Vector.empty)
      } else {
        val relevantReceivedOutputsForTx: Vector[OutputWithIndex] = {

          filteredOutputs.map { case (o, idx) =>
            OutputWithIndex(o, idx)
          }.toVector
        }

        val spks = relevantReceivedOutputsForTx.map(_.output.scriptPubKey)
        val spksInDbA = addressDAO.findByScriptPubKeysAction(spks)

        val ourOutputsA = for {
          spksInDb <- spksInDbA
        } yield {
          relevantReceivedOutputsForTx.collect {
            case OutputWithIndex(out, idx)
                if spksInDb.map(_.scriptPubKey).contains(out.scriptPubKey) =>
              OutputWithIndex(out, idx)
          }
        }

        val txDbA = for {
          ourOutputs <- ourOutputsA
          totalIncoming = ourOutputs.map(_.output.value).sum
          incomingTx <- insertIncomingTransaction(
            transaction,
            totalIncoming,
            blockHashOpt
          )
        } yield incomingTx

        val prevTagsDbA = for {
          (txDb, _) <- txDbA
          prevTagDbs <-
            addressTagDAO.findTxAction(txDb.transaction, networkParameters)
        } yield prevTagDbs

        val newTagsA = for {
          ourOutputs <- ourOutputsA
          prevTagDbs <- prevTagsDbA
          prevTags = prevTagDbs.map(_.addressTag)
          tagsToUse = prevTags
            .filterNot(tag => newTags.contains(tag)) ++ newTags
          newTagDbs = ourOutputs.flatMap { out =>
            val address = BitcoinAddress
              .fromScriptPubKey(out.output.scriptPubKey, networkParameters)
            tagsToUse.map(tag => AddressTagDb(address, tag))
          }
          created <- addressTagDAO.upsertAllAction(newTagDbs)
        } yield created

        val action = for {
          (txDb, _) <- txDbA
          ourOutputs <- ourOutputsA
          _ <- newTagsA
        } yield (ourOutputs, txDb)

        for {
          (ourOutputs, txDb) <- safeDatabase.run(action)
          utxos <- addReceivedUTXOs(ourOutputs, txDb.transaction, blockHashOpt)
        } yield utxos
      }
    }
  }
}
