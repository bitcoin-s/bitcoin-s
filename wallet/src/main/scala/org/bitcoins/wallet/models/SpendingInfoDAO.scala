package org.bitcoins.wallet.models

import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config._
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.transaction.TransactionOutput

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

  /** Marks the given outputs as spent. Assumes that all the
    * given outputs are ours, throwing if numbers aren't
    * confirming that.
    */
  def markAsSpent(
      outputs: Seq[TransactionOutput]): Future[Vector[SpendingInfoDb]] = {
    val spks = outputs.map(_.scriptPubKey)
    val filtered = table.filter(_.scriptPubKey.inSet(spks))

    for {
      utxos <- database.run(filtered.result)
      _ = assert(
        utxos.length == outputs.length,
        s"Was given ${outputs.length} outputs, found ${utxos.length} in DB")
      updated <- updateAll(utxos.map(_.copyWithSpent(spent = true)).toVector)
    } yield {
      assert(utxos.length == updated.length,
             "Updated a different number of UTXOs than what we found!")
      logger.debug(s"Marked ${updated.length} UTXO(s) as spent")
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

  /** Enumerates all unspent TX outputs in the wallet */
  def findAllUnspent(): Future[Vector[SpendingInfoDb]] = {
    val query = table.filter(!_.spent)

    database.run(query.result).map(_.toVector)
  }
}
