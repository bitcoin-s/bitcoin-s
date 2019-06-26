package org.bitcoins.wallet.models

import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.db.CRUDAutoInc
import slick.jdbc.JdbcProfile
import scala.concurrent.Future
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.crypto.DoubleSha256DigestBE

/**
  * DAO for incoming transaction outputs
  */
final case class IncomingTxoDAO(profile: JdbcProfile)(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUDAutoInc[IncomingWalletTXO] {

  import profile.api._
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table = TableQuery[IncomingTXOTable]
  private val addrTable = TableQuery[AddressTable]
  private val spendingInfoDb = TableQuery[SpendingInfoTable]

  /**
    * Given a TXID, fetches all incoming TXOs and the address the TXO pays to
    */
  def withAddress(txid: DoubleSha256DigestBE): Future[
    Vector[(IncomingWalletTXO, AddressDb)]] = {
    val query = {
      val filtered = table.filter(_.txid === txid)
      filtered join addrTable on (_.scriptPubKey === _.scriptPubKey)
    }

    database.runVec(query.result)
  }

  /**
    * Fetches all the incoming TXOs in our DB that are in
    * given TX
    */
  def findTx(tx: Transaction): Future[Vector[IncomingWalletTXO]] =
    findTx(tx.txIdBE)

  def findTx(txid: DoubleSha256DigestBE): Future[Vector[IncomingWalletTXO]] = {
    val filtered = table filter (_.txid === txid)
    database.runVec(filtered.result)
  }

  def findAllWithSpendingInfo(): Future[
    Vector[(IncomingWalletTXO, SpendingInfoDb)]] = {
    val joined = (table join spendingInfoDb).result

    database.runVec(joined)
  }

}
