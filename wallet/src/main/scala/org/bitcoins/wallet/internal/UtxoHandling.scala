package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WPKHWitnessV0}
import org.bitcoins.core.protocol.transaction.{
  CoinbaseInput,
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

  private[wallet] def updateUtxoSpentConfirmedStates(
      txo: SpendingInfoDb): Future[Option[SpendingInfoDb]] = {
    updateUtxoSpentConfirmedStates(Vector(txo)).map(_.headOption)
  }

  private[wallet] def updateUtxoSpentConfirmedStates(
      txos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    updateUtxoStates(txos, UtxoHandling.updateSpentTxoWithConfs)
  }

  private[wallet] def updateUtxoReceiveConfirmedStates(
      txo: SpendingInfoDb): Future[Option[SpendingInfoDb]] = {
    updateUtxoReceiveConfirmedStates(Vector(txo))
      .map(_.headOption)
  }

  private[wallet] def updateUtxoReceiveConfirmedStates(
      txos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    updateUtxoStates(txos, UtxoHandling.updateReceivedTxoWithConfs)
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
          case _: ReceivedState | ImmatureCoinbase | Reserved |
              BroadcastReceived =>
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

  /** Updates all the given SpendingInfoDbs to the correct state
    * based on how many confirmations they have received
    * @param spendingInfoDbs the utxos we need to update
    * @param fn the function used to transition the [[TxoState]] given a utxo and number of confirmations
    */
  private def updateUtxoStates(
      spendingInfoDbs: Vector[SpendingInfoDb],
      fn: (SpendingInfoDb, Int, Int) => SpendingInfoDb): Future[
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
              txos.map(fn(_, confs, walletConfig.requiredConfirmations))
          }
        case (None, txos) =>
          logger.debug(
            s"Currently have ${txos.size} transactions in the mempool")
          txos
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
  protected def writeUtxo(
      tx: Transaction,
      blockHashOpt: Option[DoubleSha256DigestBE],
      output: TransactionOutput,
      outPoint: TransactionOutPoint,
      addressDb: AddressDb): Future[SpendingInfoDb] = {
    val confirmationsF: Future[Int] = blockHashOpt match {
      case Some(blockHash) =>
        chainQueryApi
          .getNumberOfConfirmations(blockHash)
          .map {
            case Some(confs) =>
              confs
            case None =>
              sys.error(
                s"Could not find block with our chain data source, hash=${blockHash}")
          }
      case None =>
        Future.successful(0) //no confirmations on the tx
    }

    val stateF: Future[TxoState] = confirmationsF.map { confs =>
      if (
        tx.inputs.head
          .isInstanceOf[CoinbaseInput] && confs <= Consensus.coinbaseMaturity
      ) {
        TxoState.ImmatureCoinbase
      } else {
        UtxoHandling.getReceiveConfsState(confs,
                                          walletConfig.requiredConfirmations)
      }
    }

    val utxoF: Future[SpendingInfoDb] = stateF.map { state =>
      addressDb match {
        case segwitAddr: SegWitAddressDb =>
          SegwitV0SpendingInfo(
            state = state,
            outPoint = outPoint,
            output = output,
            privKeyPath = segwitAddr.path,
            scriptWitness = segwitAddr.witnessScript,
            spendingTxIdOpt = None
          )
        case LegacyAddressDb(path, _, _, _, _) =>
          LegacySpendingInfo(state = state,
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
            state = state,
            spendingTxIdOpt = None,
            id = None
          )
      }
    }

    for {
      utxo <- utxoF
      written <- spendingInfoDAO.createUnless(utxo) {
        (foundUtxo, utxoToCreate) =>
          foundUtxo.state.isInstanceOf[SpentState] && utxoToCreate.state
            .isInstanceOf[ReceivedState]
      }
    } yield {
      val writtenOut = written.outPoint
      logger.info(
        s"Successfully inserted UTXO ${writtenOut.txIdBE.hex}:${writtenOut.vout.toInt} amt=${output.value} into DB")
      logger.debug(s"UTXO details: ${written.output}")
      written
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
      updatedConfirmed <- updateUtxoReceiveConfirmedStates(updatedUtxos)

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
      receivedUtxos = infos.filter(_.state.isInstanceOf[ReceivedState])
      spentUtxos = infos.filter(_.state.isInstanceOf[SpentState])
      updatedReceivedInfos <- updateUtxoReceiveConfirmedStates(receivedUtxos)
      updatedSpentInfos <- updateUtxoSpentConfirmedStates(spentUtxos)
    } yield (updatedReceivedInfos ++ updatedSpentInfos).toVector
  }
}

object UtxoHandling {

  /** Updates the SpendingInfoDb to the correct state based
    * on the number of confirmations it has received
    */
  def updateReceivedTxoWithConfs(
      txo: SpendingInfoDb,
      confs: Int,
      requiredConfirmations: Int): SpendingInfoDb = {
    txo.state match {
      case TxoState.ImmatureCoinbase =>
        if (confs > Consensus.coinbaseMaturity) {
          if (confs >= requiredConfirmations)
            txo.copyWithState(TxoState.ConfirmedReceived)
          else
            txo.copyWithState(TxoState.PendingConfirmationsReceived)
        } else txo
      case TxoState.PendingConfirmationsReceived | BroadcastReceived |
          TxoState.ConfirmedReceived =>
        val state = getReceiveConfsState(confs, requiredConfirmations)
        txo.copyWithState(state)
      case TxoState.Reserved =>
        //do nothing if we have reserved the utxo
        txo
      case state: SpentState =>
        sys.error(s"Cannot update spendingInfoDb in spent state=$state")

    }
  }

  /** Given a number of confirmations and the required confirmations for the wallet
    * this method returns the appropriate [[ReceivedState]] for the number of confirmations
    */
  def getReceiveConfsState(
      confs: Int,
      requireConfirmations: Int): ReceivedState = {
    if (confs < 0) {
      sys.error(
        s"Cannot have negative confirmations, got=$confs. Did the block get reorged or exist?")
    } else if (confs == 0) {
      TxoState.BroadcastReceived
    } else if (confs >= requireConfirmations) {
      TxoState.ConfirmedReceived
    } else {
      TxoState.PendingConfirmationsReceived
    }
  }

  def updateSpentTxoWithConfs(
      txo: SpendingInfoDb,
      confs: Int,
      requiredConfirmations: Int): SpendingInfoDb = {
    txo.state match {
      case TxoState.ImmatureCoinbase =>
        sys.error(
          s"Cannot update txo with received state=${TxoState.ImmatureCoinbase}")
      case TxoState.Reserved | TxoState.PendingConfirmationsSpent |
          TxoState.ConfirmedSpent | TxoState.BroadcastSpent |
          TxoState.PendingConfirmationsReceived | TxoState.BroadcastReceived |
          TxoState.ConfirmedReceived =>
        if (confs >= requiredConfirmations) {
          txo.copyWithState(TxoState.ConfirmedSpent)
        } else if (confs == 0) {
          txo.copyWithState(TxoState.BroadcastSpent)
        } else {
          txo.copyWithState(TxoState.PendingConfirmationsSpent)
        }
    }
  }
}
