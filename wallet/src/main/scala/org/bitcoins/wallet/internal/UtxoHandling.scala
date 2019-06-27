package org.bitcoins.wallet.internal

import org.bitcoins.wallet.LockedWallet
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import scala.concurrent.Future
import org.bitcoins.wallet.models.AddressDb
import org.bitcoins.wallet.models.SpendingInfoDb
import org.bitcoins.wallet.models.SegWitAddressDb
import org.bitcoins.wallet.models.SegwitV0SpendingInfo
import org.bitcoins.wallet.models.LegacyAddressDb
import org.bitcoins.wallet.models.LegacySpendingInfo
import org.bitcoins.wallet.models.NestedSegWitAddressDb
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.wallet.api.AddUtxoResult
import org.bitcoins.core.number.UInt32
import org.bitcoins.wallet.api.AddUtxoError
import org.bitcoins.core.util.EitherUtil
import org.bitcoins.wallet.api.AddUtxoSuccess
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.BitcoinAddress
import scala.util.Success
import scala.util.Failure

/**
  * Provides functionality related to handling UTXOs in our wallet.
  * The most notable examples of functioanlity here are enumerating
  * UTXOs in the wallet and importing a UTXO into the wallet for later
  * spending.
  */
private[wallet] trait UtxoHandling { self: LockedWallet =>

  override def listUtxos(): Future[Vector[SpendingInfoDb]] =
    utxoDAO.findAll()

  /**
    * Tries to convert the provided spk to an address, and then checks if we have
    * it in our address table
    */
  private def findAddress(
      spk: ScriptPubKey): Future[Either[AddUtxoError, AddressDb]] =
    BitcoinAddress.fromScriptPubKey(spk, networkParameters) match {
      case Success(address) =>
        addressDAO.findAddress(address).map {
          case Some(addrDb) => Right(addrDb)
          case None         => Left(AddUtxoError.AddressNotFound)
        }
      case Failure(_) => Future.successful(Left(AddUtxoError.BadSPK))
    }

  /** Constructs a DB level representation of the given UTXO, and persist it to disk */
  private def writeUtxo(
      output: TransactionOutput,
      outPoint: TransactionOutPoint,
      addressDb: AddressDb): Future[SpendingInfoDb] = {

    val utxo: SpendingInfoDb = addressDb match {
      case segwitAddr: SegWitAddressDb =>
        SegwitV0SpendingInfo(
          id = None,
          outPoint = outPoint,
          output = output,
          privKeyPath = segwitAddr.path,
          scriptWitness = segwitAddr.witnessScript
        )
      case LegacyAddressDb(path, _, _, _, _) =>
        LegacySpendingInfo(outPoint = outPoint,
                           output = output,
                           privKeyPath = path)
      case nested: NestedSegWitAddressDb =>
        throw new IllegalArgumentException(
          s"Bad utxo $nested. Note: nested segwit is not implemented")
    }

    utxoDAO.create(utxo).map { written =>
      val writtenOut = written.outPoint
      logger.info(
        s"Successfully inserted UTXO ${writtenOut.txId.hex}:${writtenOut.vout.toInt} into DB")
      logger.info(s"UTXO details: ${written.output}")
      written
    }
  }

  /**
    * Adds the provided UTXO to the wallet, making it
    * available for spending.
    */
  protected def addUtxo(
      transaction: Transaction,
      vout: UInt32): Future[AddUtxoResult] = {
    import AddUtxoError._
    import org.bitcoins.core.util.EitherUtil.EitherOps._

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
      val addressDbEitherF: Future[Either[AddUtxoError, AddressDb]] =
        findAddress(output.scriptPubKey)

      // insert the UTXO into the DB
      addressDbEitherF.flatMap { addressDbE =>
        val biasedE: Either[AddUtxoError, Future[SpendingInfoDb]] = for {
          addressDb <- addressDbE
        } yield writeUtxo(output, outPoint, addressDb)

        EitherUtil.liftRightBiasedFutureE(biasedE)
      } map {
        case Right(utxo) => AddUtxoSuccess(utxo)
        case Left(e)     => e
      }
    }
  }
}
