package org.bitcoins.wallet.models

import scala.concurrent.ExecutionContext
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.db.CRUDAutoInc
import slick.jdbc.JdbcProfile
import scala.concurrent.Future
import org.bitcoins.core.protocol.transaction.Transaction

final case class IncomingTransactionDAO(profile: JdbcProfile)(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUDAutoInc[IncomingTransaction] {

  import profile.api._
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table = TableQuery[IncomingTransactionTable]
  val addrTable = TableQuery[AddressTable]

  /**
    * @param tx The transaction to look for
    * @return If found, the DB representation of the given TX,
    *         along with the address it pays to
    */
  def withAddress(
      tx: Transaction): Future[Option[(IncomingTransaction, AddressDb)]] = {
    withAddress(_ === tx)
  }

  /**
    * @param rep A predicate to filter our incoming TXs on
    * @return The first TX that meets the predicate, along with
    *         the address the transaction pays to
    */
  def withAddress(pred: Rep[Transaction] => Rep[Boolean]): Future[
    Option[(IncomingTransaction, AddressDb)]] = {
    val query = {
      val filtered = table.filter(dbTx => pred(dbTx.transaction))
      filtered join addrTable on (_.scriptPubKey === _.scriptPubKey)
    }

    database.run(query.result.headOption)
  }

}
