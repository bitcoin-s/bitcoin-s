package org.bitcoins.wallet.models

import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.wallet.config.WalletAppConfig
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext

/**
  * DAO for outgoing transaction outputs
  */
final case class OutgoingTxoDAO(profile: JdbcProfile)(
    implicit val ec: ExecutionContext,
    val appConfig: WalletAppConfig)
    extends CRUDAutoInc[OutgoingWalletTXO] {
  import profile.api._

  override val table = TableQuery[OutgoingTXOTable]
}
