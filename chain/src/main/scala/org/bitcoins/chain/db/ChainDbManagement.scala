package org.bitcoins.chain.db

import org.bitcoins.chain.models.BlockHeaderTable
import org.bitcoins.db.{DbConfig, DbManagement}
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

/**
  * Responsible for creating and destroying database
  * tables inside of the Chain project.
  */
sealed abstract class ChainDbManagement extends DbManagement {

  private val chainTable: TableQuery[BlockHeaderTable] =
    TableQuery[BlockHeaderTable]

  override val allTables = List(chainTable)

  def createHeaderTable(dbConfig: DbConfig, createIfNotExists: Boolean = true)(
      implicit ec: ExecutionContext): Future[Unit] = {
    createTable(chainTable, dbConfig, createIfNotExists)
  }

  def dropHeaderTable(dbConfig: DbConfig): Future[Unit] = {
    dropTable(chainTable, dbConfig)
  }
}

object ChainDbManagement extends ChainDbManagement
