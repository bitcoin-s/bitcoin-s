package org.bitcoins.wallet.internal

import org.bitcoins.core.compat._
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.protocol.script.{
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.util.{EitherUtil, FutureUtil}
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.wallet.api.{AddUtxoError, AddUtxoResult, AddUtxoSuccess}
import org.bitcoins.wallet.models._
import org.bitcoins.wallet.{Wallet, WalletLogger}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
  * Provides functionality related to handling UTXOs in our wallet.
  * The most notable examples of functioanlity here are enumerating
  * UTXOs in the wallet and importing a UTXO into the wallet for later
  * spending.
  */
private[wallet] trait UtxoHandling extends WalletLogger {
  self: Wallet =>

  /** @inheritdoc */
  override def listUtxos(): Future[Vector[SpendingInfoDb]] = {
    listUtxos(walletConfig.defaultAccount)
  }

  override def listUtxos(
      hdAccount: HDAccount): Future[Vector[SpendingInfoDb]] = {
    spendingInfoDAO.findAllUnspentForAccount(hdAccount)
  }

  /** Returns all the utxos originating from the given outpoints */
  def listUtxos(outPoints: Vector[TransactionOutPoint]): Future[
    Vector[SpendingInfoDb]] = {
    spendingInfoDAO
      .findAll()
      .map(_.filter(spendingInfo => outPoints.contains(spendingInfo.outPoint)))
  }

  protected def updateUtxoConfirmedState(
      txo: SpendingInfoDb,
      blockHash: DoubleSha256DigestBE): Future[SpendingInfoDb] = {
    updateUtxoConfirmedStates(Vector(txo), blockHash).map(_.head)
  }

  protected def updateUtxoConfirmedStates(
      txos: Vector[SpendingInfoDb],
      blockHash: DoubleSha256DigestBE): Future[Vector[SpendingInfoDb]] = {
    for {
      confsOpt <- chainQueryApi.getNumberOfConfirmations(blockHash)
      stateChanges <- {
        confsOpt match {
          case None =>
            Future.successful(txos)
          case Some(confs) =>
            val updatedTxos = txos.map { txo =>
              txo.state match {
                case TxoState.PendingConfirmationsReceived |
                    TxoState.DoesNotExist =>
                  if (confs >= walletConfig.requiredConfirmations) {
                    txo.copyWithState(TxoState.ConfirmedReceived)
                  } else {
                    txo.copyWithState(TxoState.PendingConfirmationsReceived)
                  }
                case TxoState.PendingConfirmationsSpent =>
                  if (confs >= walletConfig.requiredConfirmations) {
                    txo.copyWithState(TxoState.ConfirmedSpent)
                  } else {
                    txo
                  }
                case TxoState.Reserved =>
                  // We should keep the utxo as reserved so it is not used in
                  // a future transaction that it should not be in
                  txo
                case TxoState.ConfirmedReceived | TxoState.ConfirmedSpent =>
                  txo
              }
            }
            spendingInfoDAO.upsertAll(updatedTxos)
        }
      }
    } yield stateChanges
  }

  /**
    * Tries to convert the provided spk to an address, and then checks if we have
    * it in our address table
    */
  private def findAddress(
      spk: ScriptPubKey): Future[CompatEither[AddUtxoError, AddressDb]] =
    BitcoinAddress.fromScriptPubKeyT(spk, networkParameters) match {
      case Success(address) =>
        addressDAO.findAddress(address).map {
          case Some(addrDb) => CompatRight(addrDb)
          case None         => CompatLeft(AddUtxoError.AddressNotFound)
        }
      case Failure(_) => Future.successful(CompatLeft(AddUtxoError.BadSPK))
    }

  /** Constructs a DB level representation of the given UTXO, and persist it to disk */
  private def writeUtxo(
      tx: Transaction,
      state: TxoState,
      output: TransactionOutput,
      outPoint: TransactionOutPoint,
      addressDb: AddressDb,
      blockHash: Option[DoubleSha256DigestBE]): Future[SpendingInfoDb] = {

    val utxo: SpendingInfoDb = addressDb match {
      case segwitAddr: SegWitAddressDb =>
        SegwitV0SpendingInfo(
          state = state,
          txid = tx.txIdBE,
          outPoint = outPoint,
          output = output,
          privKeyPath = segwitAddr.path,
          scriptWitness = segwitAddr.witnessScript,
          blockHash = blockHash
        )
      case LegacyAddressDb(path, _, _, _, _) =>
        LegacySpendingInfo(state = state,
                           txid = tx.txIdBE,
                           outPoint = outPoint,
                           output = output,
                           privKeyPath = path,
                           blockHash = blockHash)
      case nested: NestedSegWitAddressDb =>
        NestedSegwitV0SpendingInfo(
          outPoint = outPoint,
          output = output,
          privKeyPath = nested.path,
          redeemScript = P2WPKHWitnessSPKV0(nested.ecPublicKey),
          scriptWitness = P2WPKHWitnessV0(nested.ecPublicKey),
          txid = tx.txIdBE,
          state = state,
          id = None,
          blockHash = blockHash
        )
    }

    for {
      written <- spendingInfoDAO.create(utxo)
    } yield {
      val writtenOut = written.outPoint
      logger.info(
        s"Successfully inserted UTXO ${writtenOut.txId.hex}:${writtenOut.vout.toInt} into DB")
      logger.debug(s"UTXO details: ${written.output}")
      written
    }
  }

  /**
    * Adds the provided UTXO to the wallet
    */
  protected def addUtxo(
      transaction: Transaction,
      vout: UInt32,
      state: TxoState,
      blockHash: Option[DoubleSha256DigestBE]): Future[AddUtxoResult] = {
    import AddUtxoError._

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
      val addressDbEitherF: Future[CompatEither[AddUtxoError, AddressDb]] =
        findAddress(output.scriptPubKey)

      // insert the UTXO into the DB
      addressDbEitherF.flatMap { addressDbE =>
        val biasedE: CompatEither[AddUtxoError, Future[SpendingInfoDb]] = for {
          addressDb <- addressDbE
        } yield writeUtxo(tx = transaction,
                          state = state,
                          output = output,
                          outPoint = outPoint,
                          addressDb = addressDb,
                          blockHash = blockHash)

        EitherUtil.liftRightBiasedFutureE(biasedE)
      } map {
        case CompatRight(utxo) => AddUtxoSuccess(utxo)
        case CompatLeft(e)     => e
      }
    }
  }

  override def markUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    val updated = utxos.map(_.copyWithState(TxoState.Reserved))
    for {
      utxos <- spendingInfoDAO.updateAll(updated)
      _ <- walletCallbacks.executeOnReservedUtxos(logger, utxos)
    } yield utxos
  }

  override def unmarkUTXOsAsReserved(
      utxos: Vector[SpendingInfoDb]): Future[Vector[SpendingInfoDb]] = {
    val unreserved = utxos.filterNot(_.state == TxoState.Reserved)
    require(unreserved.isEmpty, s"Some utxos are not reserved, got $unreserved")

    // unmark all utxos are reserved
    val groupedUtxos = utxos
      .map(_.copyWithState(TxoState.PendingConfirmationsReceived))
      .groupBy(_.blockHash)

    val mempoolUtxos = Try(groupedUtxos(None)).getOrElse(Vector.empty)

    // get the ones in blocks
    val utxosInBlocks = groupedUtxos.map {
      case (Some(hash), utxos) =>
        Some(hash, utxos)
      case (None, _) =>
        None
    }.flatten

    for {
      updatedMempoolUtxos <- spendingInfoDAO.updateAll(mempoolUtxos)
      // update the confirmed ones
      updatedBlockUtxos <-
        FutureUtil
          .sequentially(utxosInBlocks.toVector) {
            case (hash, utxos) =>
              updateUtxoConfirmedStates(utxos, hash)
          }
      updated = updatedMempoolUtxos ++ updatedBlockUtxos.flatten
      _ <- walletCallbacks.executeOnReservedUtxos(logger, updated)
    } yield updated
  }

  /** @inheritdoc */
  override def unmarkUTXOsAsReserved(
      tx: Transaction): Future[Vector[SpendingInfoDb]] = {
    val utxosF = listUtxos()
    val utxosInTxF = for {
      utxos <- utxosF
    } yield {
      val txOutPoints = tx.inputs.map(_.previousOutput)
      utxos.filter(si => txOutPoints.contains(si.outPoint))
    }
    utxosInTxF.flatMap(unmarkUTXOsAsReserved)
  }

  /** @inheritdoc */
  override def updateUtxoPendingStates(
      blockHeader: BlockHeader): Future[Vector[SpendingInfoDb]] = {
    for {
      infos <- spendingInfoDAO.findAllPendingConfirmation
      updatedInfos <- updateUtxoConfirmedStates(infos, blockHeader.hashBE)
    } yield updatedInfos
  }
}
