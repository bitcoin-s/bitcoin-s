package org.bitcoins.node.models

import slick.jdbc.SQLiteProfile.api._
import slick.jdbc.JdbcProfile
import org.bitcoins.db.CRUDAutoInc
import org.bitcoins.node.config.NodeAppConfig
import scala.concurrent.ExecutionContext
import slick.lifted.TableQuery
import scala.concurrent.Future
import org.bitcoins.core.crypto.DoubleSha256Digest

final case class BroadcastAbleTransactionDAO(profile: JdbcProfile)(
    implicit val appConfig: NodeAppConfig,
    val ec: ExecutionContext)
    extends CRUDAutoInc[BroadcastAbleTransaction] {

  val table: TableQuery[BroadcastAbleTransactionTable] =
    TableQuery[BroadcastAbleTransactionTable]

  /** Searches for a TX by its TXID */
  def findByHash(
      hash: DoubleSha256Digest): Future[Option[BroadcastAbleTransaction]] = {
    import org.bitcoins.db.DbCommonsColumnMappers._

    val query = table.filter(_.txid === hash.flip)
    database.run(query.result).map(_.headOption)
  }

}
