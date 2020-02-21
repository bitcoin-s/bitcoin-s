package org.bitcoins.wallet.internal

import org.bitcoins.core.compat._
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
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
import org.bitcoins.core.util.EitherUtil
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.wallet.api.{AddUtxoError, AddUtxoResult, AddUtxoSuccess}
import org.bitcoins.wallet.models._
import org.bitcoins.wallet.{LockedWallet, WalletLogger}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
  * Provides functionality related to handling UTXOs in our wallet.
  * The most notable examples of functioanlity here are enumerating
  * UTXOs in the wallet and importing a UTXO into the wallet for later
  * spending.
  */
private[wallet] trait UtxoHandling extends WalletLogger {
  self: LockedWallet =>

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

  /**
    * Tries to convert the provided spk to an address, and then checks if we have
    * it in our address table
    */
  private def findAddress(
      spk: ScriptPubKey): Future[CompatEither[AddUtxoError, AddressDb]] =
    BitcoinAddress.fromScriptPubKey(spk, networkParameters) match {
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
}
