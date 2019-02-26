package org.bitcoins.node.db

import org.bitcoins.node.models.BlockHeaderTable
import slick.lifted.TableQuery

import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

object NodeDbManagement extends DbManagement {

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
