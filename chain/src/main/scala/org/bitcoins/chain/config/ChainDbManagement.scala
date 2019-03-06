package org.bitcoins.chain.config

import org.bitcoins.chain.models.BlockHeaderTable
import org.bitcoins.db.{DbConfig, DbManagement}
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.SQLiteProfile.api._

sealed abstract class ChainDbManagement extends DbManagement {
  private val blockHeaderTable = TableQuery[BlockHeaderTable]
  override val allTables = List(blockHeaderTable)

  def createBlockHeaderTable(dbConfig: DbConfig)(
      implicit ec: ExecutionContext): Future[Unit] = {
    createTable(blockHeaderTable, dbConfig)
  }

  def dropBlockHeaderTable(dbConfig: DbConfig)(
      implicit ec: ExecutionContext): Future[Unit] = {
    dropTable(blockHeaderTable, dbConfig)
  }
}

object ChainDbManagement extends ChainDbManagement
