package org.bitcoins.chain.db

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{BlockHeaderTable, CompactFilterHeaderTable, CompactFilterTable}
import org.bitcoins.db.DbManagement
import slick.lifted.TableQuery

import scala.concurrent.{ExecutionContext, Future}

/**
  * Responsible for creating and destroying database
  * tables inside of the Chain project.
  */
sealed abstract class ChainDbManagement extends DbManagement {

  private val chainTable: TableQuery[BlockHeaderTable] =
    TableQuery[BlockHeaderTable]

  private val filterHeaderTable: TableQuery[CompactFilterHeaderTable] =
    TableQuery[CompactFilterHeaderTable]

  private val filterTable: TableQuery[CompactFilterTable] =
    TableQuery[CompactFilterTable]

  override val allTables = List(chainTable, filterHeaderTable, filterTable)

  def createHeaderTable(createIfNotExists: Boolean = true)(
      implicit config: ChainAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    createTable(chainTable, createIfNotExists)
  }

}

object ChainDbManagement extends ChainDbManagement
