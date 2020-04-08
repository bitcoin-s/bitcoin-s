package org.bitcoins.node.db

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.{BroadcastAbleTransactionDAO}

import scala.concurrent.ExecutionContext

trait NodeDbManagement extends DbManagement { _: JdbcProfileComponent =>

  import profile.api._

  def ec: ExecutionContext

  override def appConfig: NodeAppConfig

  private lazy val txTable: TableQuery[Table[_]] = {
    BroadcastAbleTransactionDAO()(appConfig.asInstanceOf[NodeAppConfig],ec).table.asInstanceOf[TableQuery[Table[_]]]
  }

  override val allTables = List(txTable)

}
