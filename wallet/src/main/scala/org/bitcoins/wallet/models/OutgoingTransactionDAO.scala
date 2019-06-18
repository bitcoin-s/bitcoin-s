package org.bitcoins.wallet.models
import scala.concurrent.ExecutionContext
import slick.jdbc.JdbcProfile
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.db.CRUDAutoInc

final case class OutgoingTransactionDAO(profile: JdbcProfile)(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUDAutoInc[OutgoingTransaction] {
  import profile.api._

  override val table = TableQuery[OutgoingTransactionTable]
}
