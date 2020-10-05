package org.bitcoins.node.db

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{BroadcastAbleTransactionDAO}

import scala.concurrent.ExecutionContext

trait NodeDbManagement extends DbManagement {
  _: JdbcProfileComponent[NodeAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val txTable: TableQuery[Table[_]] = {
    BroadcastAbleTransactionDAO()(appConfig, ec).table
  }

  override lazy val allTables: List[TableQuery[Table[_]]] = List(txTable)

}
