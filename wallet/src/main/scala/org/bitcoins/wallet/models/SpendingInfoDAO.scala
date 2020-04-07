package org.bitcoins.wallet.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.hd.HDAccount
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.wallet.utxo.TxoState
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class SpendingInfoDAO()(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUDAutoInc[SpendingInfoDb] {

  import org.bitcoins.db.DbCommonsColumnMappers._

  /** The table inside our database we are inserting into */
  override val table = TableQuery[SpendingInfoTable]
  private val addrTable = TableQuery[AddressTable]

  /**
    * Fetches all the incoming TXOs in our DB that are in
    * the given TX
    */
  def findTx(tx: Transaction): Future[Vector[SpendingInfoDb]] =
    findTx(tx.txIdBE)

  /**
    * Finds all the outputs being spent in the given
    * transaction
    */
  def findOutputsBeingSpent(tx: Transaction): Future[Seq[SpendingInfoDb]] = {

    val filtered = table
      .filter {
        case txo =>
          txo.outPoint.inSet(tx.inputs.map(_.previousOutput))
      }

    database.run(filtered.result)
  }

  /**
    * Given a TXID, fetches all incoming TXOs and the address the TXO pays to
    */
  def withAddress(txid: DoubleSha256DigestBE): Future[
    Vector[(SpendingInfoDb, AddressDb)]] = {
    val query = {
      val filtered = table.filter(_.txid === txid)
      filtered.join(addrTable).on(_.scriptPubKey === _.scriptPubKey)
    }

    database.runVec(query.result)
  }

  /** Updates the [[org.bitcoins.core.wallet.utxo.TxoState TxoState]] of all of the given
    * outputs in our database to be the state
    */
  def updateTxoState(
      outputs: Seq[TransactionOutput],
      state: TxoState): Future[Vector[SpendingInfoDb]] = {
    val spks = outputs.map(_.scriptPubKey)
    val filtered = table.filter(_.scriptPubKey.inSet(spks))

    for {
      utxos <- database.run(filtered.result)
      _ = require(
        utxos.length == outputs.length,
        s"Was given ${outputs.length} outputs, found ${utxos.length} in DB")
      newStates = utxos.map(_.copyWithState(state = state)).toVector
      updated <- updateAll(newStates)
    } yield {
      require(utxos.length == updated.length,
              "Updated a different number of UTXOs than what we found!")
      logger.debug(s"Updated ${updated.length} UTXO(s) to state=${state}")
      updated

    }

  }

  /**
    * Fetches all the incoming TXOs in our DB that are in
    * the transaction with the given TXID
    */
  def findTx(txid: DoubleSha256DigestBE): Future[Vector[SpendingInfoDb]] = {
    val filtered = table.filter(_.txid === txid)
    database.runVec(filtered.result)
  }

  def findByScriptPubKey(
      scriptPubKey: ScriptPubKey): Future[Vector[SpendingInfoDb]] = {
    val filtered = table.filter(_.scriptPubKey === scriptPubKey)
    database.runVec(filtered.result)
  }

  private val receivedStates: Set[TxoState] =
    Set(TxoState.PendingConfirmationsReceived, TxoState.ConfirmedReceived)

  /** Enumerates all unspent TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or [[TxoState.ConfirmedReceived]] */
  def findAllUnspent(): Future[Vector[SpendingInfoDb]] = {
    val query = table.filter(_.state.inSet(receivedStates))

    database.run(query.result).map(_.toVector)
  }

  /** Finds all utxos for a given account */
  def findAllUnspentForAccount(
      hdAccount: HDAccount): Future[Vector[SpendingInfoDb]] = {
    val allUtxosF = findAllUnspent()
    allUtxosF.map { allUtxos =>
      allUtxos.filter(
        utxo =>
          HDAccount.isSameAccount(bip32Path = utxo.privKeyPath,
                                  account = hdAccount))
    }
  }

  /** Enumerates all TX outputs in the wallet with the state
    * [[TxoState.PendingConfirmationsReceived]] or [[TxoState.PendingConfirmationsSpent]] */
  def findAllPendingConfirmation: Future[Vector[SpendingInfoDb]] = {
    val query = table.filter(_.state.inSet(TxoState.pendingConfStates))

    database.run(query.result).map(_.toVector)
  }

  /** Enumerates all TX outpoints in the wallet */
  def findAllOutpoints(): Future[Vector[TransactionOutPoint]] = {
    val query = table.map(_.outPoint)
    database.runVec(query.result).map(_.toVector)
  }
}
