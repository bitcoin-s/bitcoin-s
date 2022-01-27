package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.AddUtxoError._
import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.api.wallet.{
  AddUtxoError,
  AddUtxoResult,
  AddUtxoSuccess
}
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WPKHWitnessV0}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.BlockHashWithConfs
import org.bitcoins.core.wallet.utxo.TxoState._
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.Future

/** Provides functionality related to handling UTXOs in our wallet.
  * The most notable examples of functionality here are enumerating
  * UTXOs in the wallet and importing a UTXO into the wallet for later
  * spending.
  */
private[wallet] trait UtxoHandling extends WalletLogger {
  self: Wallet =>

  /** @inheritdoc */
  def listDefaultAccountUtxos(): Future[Vector[SpendingInfoDb]] =
    listUtxos(walletConfig.defaultAccount)

  /** @inheritdoc */
  override def listUtxos(): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findAllUnspent()
  }

  override def listUtxos(
      hdAccount: HDAccount): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findAllUnspentForAccount(hdAccount)
  }

  /** Returns all the utxos originating from the given outpoints */
  def listUtxos(outPoints: Vector[TransactionOutPoint]): Future[
    Vector[SpendingInfoDb]] = {
    spendingInfoDAO
      .findAllSpendingInfos()
      .map(_.filter(spendingInfo => outPoints.contains(spendingInfo.outPoint)))
  }

  override def listUtxos(tag: AddressTag): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findAllUnspentForTag(tag)
  }

  override def listUtxos(
      hdAccount: HDAccount,
      tag: AddressTag): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findAllUnspentForTag(tag).map { utxos =>
      utxos.filter(utxo =>
        HDAccount.isSameAccount(bip32Path = utxo.privKeyPath,
                                account = hdAccount))
    }
  }

  override def listUtxos(state: TxoState): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findByTxoState(state)
  }

  override def listUtxos(
      hdAccount: HDAccount,
      state: TxoState): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findByTxoState(state).map { utxos =>
      utxos.filter(utxo =>
        HDAccount.isSameAccount(bip32Path = utxo.privKeyPath,
                                account = hdAccount))
    }
  }

  private[wallet] def updateUtxoConfirmedState(
      txo: SpendingInfoDb): Future[Option[SpendingInfoDb]] = {
    updateUtxoConfirmedStates(Vector(txo)).map(_.headOption)
  }

  /** Returns a map of the SpendingInfoDbs with their relevant block.
    * If the block hash is None, then it is a mempool transaction.
    * The relevant block is determined by if the utxo has been spent or not.
    * If it has been spent it uses the block that included the spending transaction,
    * otherwise it uses the block that included the receiving transaction.
    */
  private[wallet] def getDbsByRelevantBlock(
      spendingInfoDbs: Vector[SpendingInfoDb]): Future[
    Map[Option[DoubleSha256DigestBE], Vector[SpendingInfoDb]]] = {
    val txIds =
      spendingInfoDbs.map { db =>
        db.spendingTxIdOpt match {
          case Some(spendingTxId) =>
            spendingTxId
          case None =>
            db.txid
        }
      }

    transactionDAO.findByTxIdBEs(txIds).map { txDbs =>
      val blockHashMap = txDbs.map(db => db.txIdBE -> db.blockHashOpt).toMap
      val blockHashAndDb = spendingInfoDbs.map { txo =>
        val txToUse = txo.state match {
          case _: ReceivedState | DoesNotExist | ImmatureCoinbase |
              Reserved | BroadcastReceived =>
            txo.txid
          case PendingConfirmationsSpent | ConfirmedSpent | BroadcastSpent =>
            txo.spendingTxIdOpt.get
        }
        (blockHashMap(txToUse), txo)
      }
      blockHashAndDb.groupBy(_._1).map { case (blockHashOpt, vec) =>
        blockHashOpt -> vec.map(_._2)
      }
    }
  }

  /** Updates the SpendingInfoDb to the correct state based
    * on the number of confirmations it has received
    */
  private[wallet] def updateTxoWithConfs(
      txo: SpendingInfoDb,
      confs: Int): SpendingInfoDb = {
    txo.state match {
      case TxoState.ImmatureCoinbase =>
        if (confs > Consensus.coinbaseMaturity) {
          if (confs >= walletConfig.requiredConfirmations)
            txo.copyWithState(TxoState.ConfirmedReceived)
          else
            txo.copyWithState(TxoState.PendingConfirmationsReceived)
        } else txo
      case TxoState.PendingConfirmationsReceived | BroadcastReceived =>
        if (confs >= walletConfig.requiredConfirmations)
          txo.copyWithState(TxoState.ConfirmedReceived)
        else txo.copyWithState(PendingConfirmationsReceived)
      case TxoState.PendingConfirmationsSpent | BroadcastSpent =>
        if (confs >= walletConfig.requiredConfirmations)
          txo.copyWithState(TxoState.ConfirmedSpent)
        else txo.copyWithState(PendingConfirmationsSpent)
      case TxoState.Reserved =>
        // We should keep the utxo as reserved so it is not used in
        // a future transaction that it should not be in
        txo
      case TxoState.DoesNotExist | TxoState.ConfirmedReceived |
          TxoState.ConfirmedSpent =>
        txo
    }
  }

  /** Updates all the given SpendingInfoDbs to the correct state
    * based on how many confirmations they have received
    */
  private[wallet] def updateUtxoConfirmedStates(
      spendingInfoDbs: Vector[SpendingInfoDb]): Future[
    Vector[SpendingInfoDb]] = {
    val relevantBlocksF: Future[
      Map[Option[DoubleSha256DigestBE], Vector[SpendingInfoDb]]] = {
      getDbsByRelevantBlock(spendingInfoDbs)
    }

    //fetch all confirmations for those blocks, do it in parallel
    //as an optimzation, previously we would fetch sequentially
    val blocksWithConfsF: Future[
      Map[Option[BlockHashWithConfs], Vector[SpendingInfoDb]]] = {
      for {
        relevantBlocks <- relevantBlocksF
        blocksWithConfirmations <- getConfirmationsForBlocks(relevantBlocks)
      } yield blocksWithConfirmations
    }

    val toUpdateF = blocksWithConfsF.map { txsByBlock =>
      val toUpdateFs: Vector[SpendingInfoDb] = txsByBlock.flatMap {
        case (Some(blockHashWithConfs), txos) =>
          blockHashWithConfs.confirmationsOpt match {
            case None =>
              logger.warn(
                s"Given txos exist in block (${blockHashWithConfs.blockHash.hex}) that we do not have or that has been reorged! $txos")
              Vector.empty
            case Some(confs) =>
              txos.map(updateTxoWithConfs(_, confs))
          }
        case (None, txos) =>
          logger.debug(
            s"Currently have ${txos.size} transactions in the mempool")
          Vector.empty
      }.toVector

      toUpdateFs
    }

    for {
      toUpdate <- toUpdateF
      _ =
        if (toUpdate.nonEmpty)
          logger.info(s"${toUpdate.size} txos are now confirmed!")
        else logger.trace("No txos to be confirmed")
      updated <- spendingInfoDAO.upsertAllSpendingInfoDb(toUpdate)
    } yield updated
  }

  /** Fetches confirmations for the given blocks in parallel */
  private def getConfirmationsForBlocks(
      relevantBlocks: Map[
        Option[DoubleSha256DigestBE],
        Vector[SpendingInfoDb]]): Future[
    Map[Option[BlockHashWithConfs], Vector[SpendingInfoDb]]] = {

    val blockHashesWithConfsVec = relevantBlocks.map {
      case (blockHashOpt, spendingInfoDbs) =>
        blockHashOpt match {
          case Some(blockHash) =>
            chainQueryApi
              .getNumberOfConfirmations(blockHash)
              .map(confs => Some(BlockHashWithConfs(blockHash, confs)))
              .map(blockWithConfsOpt => (blockWithConfsOpt, spendingInfoDbs))
          case None =>
            Future.successful((None, spendingInfoDbs))
        }
    }

    Future
      .sequence(blockHashesWithConfsVec)
      .map(_.toMap)
  }

  /** Constructs a DB level representation of the given UTXO, and persist it to disk */
  private def writeUtxo(
      tx: Transaction,
      state: ReceivedState,
      output: TransactionOutput,
      outPoint: TransactionOutPoint,
      addressDb: AddressDb): Future[SpendingInfoDb] = {

    val utxo: SpendingInfoDb = addressDb match {
      case segwitAddr: SegWitAddressDb =>
        SegwitV0SpendingInfo(
          state = state,
          txid = tx.txIdBE,
          outPoint = outPoint,
          output = output,
          privKeyPath = segwitAddr.path,
          scriptWitness = segwitAddr.witnessScript,
          spendingTxIdOpt = None
        )
      case LegacyAddressDb(path, _, _, _, _) =>
        LegacySpendingInfo(state = state,
                           txid = tx.txIdBE,
                           outPoint = outPoint,
                           output = output,
                           privKeyPath = path,
                           spendingTxIdOpt = None)
      case nested: NestedSegWitAddressDb =>
        NestedSegwitV0SpendingInfo(
          outPoint = outPoint,
          output = output,
          privKeyPath = nested.path,
          redeemScript = P2WPKHWitnessSPKV0(nested.ecPublicKey),
          scriptWitness = P2WPKHWitnessV0(nested.ecPublicKey),
          txid = tx.txIdBE,
          state = state,
          spendingTxIdOpt = None,
          id = None
        )
    }

    for {
      written <- spendingInfoDAO.create(utxo)
    } yield {
      val writtenOut = written.outPoint
      logger.info(
        s"Successfully inserted UTXO ${writtenOut.txIdBE.hex}:${writtenOut.vout.toInt} amt=${output.value} into DB")
      logger.debug(s"UTXO details: ${written.output}")
      written
    }
  }

  /** Adds the provided UTXO to the wallet
    */
  protected def addUtxo(
      transaction: Transaction,
      vout: UInt32,
      state: ReceivedState,
      addressDbE: Either[AddUtxoError, AddressDb]): Future[AddUtxoResult] = {

    if (vout.toInt >= transaction.outputs.length) {
      //out of bounds output
      Future.successful(VoutIndexOutOfBounds)
    } else {
      val output = transaction.outputs(vout.toInt)
      val outPoint = TransactionOutPoint(transaction.txId, vout)

      // insert the UTXO into the DB
      val insertedUtxoEF: Either[AddUtxoError, Future[SpendingInfoDb]] = for {
        addressDb <- addressDbE
      } yield writeUtxo(tx = transaction,
                        state = state,
                        output = output,
                        outPoint = outPoint,
                        addressDb = addressDb)
      insertedUtxoEF match {
        case Right(utxoF) => utxoF.map(AddUtxoSuccess)
        case Left(e)      => Future.successful(e)
      }
    }
  }

  override def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    val outPoints = utxos.map(_.outPoint)
    logger.info(s"Reserving utxos=$outPoints")
    val updated = utxos.map(_.copyWithState(TxoState.Reserved))
    for {
      utxos <- spendingInfoDAO.markAsReserved(updated)
      _ <- walletCallbacks.executeOnReservedUtxos(logger, utxos)
    } yield utxos
  }

  /** @inheritdoc */
  override def markUTXOsAsReserved(
      tx: Transaction): Future[Vector[SpendingInfoDb]] = {
    for {
      utxos <- spendingInfoDAO.findOutputsBeingSpent(tx)
      reserved <- markUTXOsAsReserved(utxos)
    } yield reserved
  }

  override def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    logger.info(s"Unreserving utxos ${utxos.map(_.outPoint)}")
    val updatedUtxosF = Future {
      //make sure exception isn't thrown outside of a future to fix
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/3813
      val unreserved = utxos.filterNot(_.state == TxoState.Reserved)
      require(unreserved.isEmpty,
              s"Some utxos are not reserved, got $unreserved")

      // unmark all utxos are reserved
      val updatedUtxos = utxos
        .map(_.copyWithState(TxoState.PendingConfirmationsReceived))
      updatedUtxos
    }

    for {
      updatedUtxos <- updatedUtxosF
      // update the confirmed utxos
      updatedConfirmed <- updateUtxoConfirmedStates(updatedUtxos)

      // update the utxos that are in blocks but not considered confirmed yet
      pendingConf = updatedUtxos.filterNot(utxo =>
        updatedConfirmed.exists(_.outPoint == utxo.outPoint))
      updated <- spendingInfoDAO.updateAllSpendingInfoDb(
        pendingConf ++ updatedConfirmed)

      _ <- walletCallbacks.executeOnReservedUtxos(logger, updated)
    } yield updated
  }

  /** @inheritdoc */
  override def unmarkUTXOsAsReserved(
      tx: Transaction): Future[Vector[SpendingInfoDb]] = {
    for {
      utxos <- spendingInfoDAO.findOutputsBeingSpent(tx)
      reserved = utxos.filter(_.state == TxoState.Reserved)
      updated <- unmarkUTXOsAsReserved(reserved.toVector)
    } yield updated
  }

  /** @inheritdoc */
  override def updateUtxoPendingStates(): Future[Vector[SpendingInfoDb]] = {
    for {
      infos <- spendingInfoDAO.findAllPendingConfirmation
      _ = logger.debug(s"Updating states of ${infos.size} pending utxos...")
      updatedInfos <- updateUtxoConfirmedStates(infos)
    } yield updatedInfos
  }
}
