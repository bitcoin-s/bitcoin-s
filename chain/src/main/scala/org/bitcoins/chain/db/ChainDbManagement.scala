package org.bitcoins.chain.db

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.db.{DbManagement, JdbcProfileComponent}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Responsible for creating and destroying database
  * tables inside of the Chain project.
  */
trait ChainDbManagement extends DbManagement {
  _: JdbcProfileComponent[ChainAppConfig] =>
  import profile.api._

  def ec: ExecutionContext

  private lazy val chainTable: TableQuery[Table[_]] =
    BlockHeaderDAO()(ec, appConfig).table

  private lazy val filterHeaderTable: TableQuery[Table[_]] = {
    CompactFilterHeaderDAO()(ec, appConfig).table
  }

  private lazy val filterTable: TableQuery[Table[_]] = {
    CompactFilterDAO()(ec, appConfig).table
  }

  override lazy val allTables: List[TableQuery[Table[_]]] =
    List(chainTable, filterHeaderTable, filterTable)

  def createHeaderTable(createIfNotExists: Boolean = true): Future[Unit] = {
    createTable(chainTable, createIfNotExists)(ec)
  }

}
