package org.bitcoins.wallet.internal

import org.bitcoins.wallet.LockedWallet
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.wallet.models._
import scala.concurrent.Future
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.wallet.api.AddUtxoSuccess
import org.bitcoins.wallet.api.AddUtxoError
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.db.KeyHandlingLogger

/** Provides functionality for processing transactions. This
  * includes importing UTXOs spent to our wallet, updating
  * confirmation counts and marking UTXOs as spent when
  * spending from our wallet
  */
private[wallet] trait TransactionProcessing extends KeyHandlingLogger {
  self: LockedWallet =>
  /////////////////////
  // Public facing API

  /** @inheritdoc */
  override def processTransaction(
      transaction: Transaction,
      confirmations: Int): Future[LockedWallet] = {
    logger.info(
      s"Processing transaction=${transaction.txIdBE} with confirmations=$confirmations")
    processTransactionImpl(transaction, confirmations).map {
      case ProcessTxResult(incoming, outgoing) =>
        logger.info(
          s"Finished processing of transaction=${transaction.txIdBE}. Relevant incomingTXOs=${incoming.length}, outgoingTXOs=${outgoing.length}")
        this
    }

  }

  private[wallet] case class ProcessTxResult(
      updatedIncoming: List[SpendingInfoDb],
      updatedOutgoing: List[SpendingInfoDb])

  /////////////////////
  // Internal wallet API

  /**
    * Processes TXs originating from our wallet.
    * This is called right after we've signed a TX,
    * updating our UTXO state.
    */
  private[wallet] def processOurTransaction(
      transaction: Transaction,
      confirmations: Int): Future[ProcessTxResult] = {
    logger.info(
      s"Processing TX from our wallet, transaction=${transaction.txIdBE} with confirmations=$confirmations")
    processTransactionImpl(transaction, confirmations).map { result =>
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

  /** Does the grunt work of processing a TX.
    * This is called by either the internal or public TX
    * processing method, which logs and transforms the
    * output fittingly.
    */
  private def processTransactionImpl(
      transaction: Transaction,
      confirmations: Int): Future[ProcessTxResult] = {

    val incomingTxoFut: Future[Vector[SpendingInfoDb]] =
      spendingInfoDAO
        .findTx(transaction)
        .flatMap {
          // no existing elements found
          case Vector() =>
            processNewIncomingTx(transaction, confirmations).map(_.toVector)

          case txos: Vector[SpendingInfoDb] =>
            val txoProcessingFutures =
              txos
                .map(processExistingIncomingTxo(transaction, confirmations, _))

            Future
              .sequence(txoProcessingFutures)

        }

    val outgoingTxFut: Future[Vector[SpendingInfoDb]] = {
      for {
        outputsBeingSpent <- spendingInfoDAO.findOutputsBeingSpent(transaction)
        processed <- FutureUtil.sequentially(outputsBeingSpent)(
          markAsSpentIfUnspent)
      } yield processed.flatten.toVector

    }

    val aggregateFut =
      for {
        incoming <- incomingTxoFut
        outgoing <- outgoingTxFut
      } yield {
        ProcessTxResult(incoming.toList, outgoing.toList)
      }

    aggregateFut.failed.foreach { err =>
      val msg = s"Error when processing transaction=${transaction.txIdBE}"
      logger.error(msg, err)
    }

    aggregateFut
  }

  /** If the given UTXO is marked as unspent, updates
    * its spending status. Otherwise returns `None`.
    */
  private val markAsSpentIfUnspent: SpendingInfoDb => Future[
    Option[SpendingInfoDb]] = { out =>
    if (out.spent) {
      Future.successful(None)
    } else {
      val updatedF =
        spendingInfoDAO.update(out.copyWithSpent(spent = true))
      updatedF.foreach(
        updated =>
          logger.debug(
            s"Marked utxo=${updated.toHumanReadableString} as spent=${updated.spent}")
      )
      updatedF.map(Some(_))
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
      spent: Boolean,
      confirmations: Int): Future[SpendingInfoDb] =
    addUtxo(transaction,
            UInt32(index),
            spent = spent,
            confirmations = confirmations)
      .flatMap {
        case AddUtxoSuccess(utxo) => Future.successful(utxo)
        case err: AddUtxoError =>
          logger.error(s"Could not add UTXO", err)
          Future.failed(err)
      }

  private case class OutputWithIndex(output: TransactionOutput, index: Int)

  /**
    * Processes an incoming transaction that already exists in our wallet.
    * If the incoming transaction has more confirmations than what we
    * have in the DB, we update the TX
    */
  private def processExistingIncomingTxo(
      transaction: Transaction,
      confirmations: Int,
      foundTxo: SpendingInfoDb): Future[SpendingInfoDb] = {
    if (foundTxo.confirmations < confirmations) {
      // TODO The assumption here is that double-spends never occur. That's not
      // the case. This must be fixed when double-spend logic is implemented.
      logger.debug(
        s"Increasing confirmation count of txo=${transaction.txIdBE}, old=${foundTxo.confirmations} new=${confirmations}")
      val updateF =
        spendingInfoDAO.update(
          foundTxo.copyWithConfirmations(confirmations = confirmations))

      updateF.foreach(tx =>
        logger.debug(
          s"Updated confirmation count=${tx.confirmations} of output=${foundTxo}"))
      updateF.failed.foreach(err =>
        logger.error(
          s"Failed to update confirmation count of transaction=${transaction.txIdBE}",
          err))

      updateF
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
      if (foundTxo.txid == transaction.txIdBE) {
        logger.debug(
          s"Skipping further processing of transaction=${transaction.txIdBE}, already processed.")
        Future.successful(foundTxo)
      } else {
        val errMsg =
          Seq(
            s"Found TXO has txid=${foundTxo.txid}, tx we were given has txid=${transaction.txIdBE}.",
            "This is either a reorg or a double spent, which is not implemented yet"
          ).mkString(" ")
        logger.error(errMsg)
        Future.failed(new RuntimeException(errMsg))
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
      confirmations: Int): Future[Seq[SpendingInfoDb]] = {
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

          val addUTXOsFut: Future[Seq[SpendingInfoDb]] =
            Future
              .sequence {
                xs.map(
                  out =>
                    processUtxo(transaction,
                                out.index,
                                confirmations = confirmations,
                                // TODO is this correct?
                                spent = false))
              }

          addUTXOsFut

      }
    }
  }
}
