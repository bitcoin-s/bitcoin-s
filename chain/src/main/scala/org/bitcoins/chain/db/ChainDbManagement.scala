package org.bitcoins.chain.db

import org.bitcoins.db._
import org.bitcoins.chain.models.BlockHeaderTable
import org.bitcoins.db.{DbManagement}
import slick.lifted.TableQuery

import scala.concurrent.Future

/**
  * Responsible for creating and destroying database
  * tables inside of the Chain project.
  */
sealed abstract class ChainDbManagement extends DbManagement {

  private val chainTable: TableQuery[BlockHeaderTable] =
    TableQuery[BlockHeaderTable]

  override val allTables = List(chainTable)

  def createHeaderTable(createIfNotExists: Boolean = true)(
      implicit config: AppConfig): Future[Unit] = {
    createTable(chainTable, createIfNotExists)
  }

  def dropHeaderTable()(implicit config: AppConfig): Future[Unit] = {
    dropTable(chainTable)
  }
}

object ChainDbManagement extends ChainDbManagement
