package org.bitcoins.chain.db

import org.bitcoins.db._
import org.bitcoins.chain.models.{BlockHeaderTable, CompactFilterHeaderTable}
import org.bitcoins.db.DbManagement
import slick.lifted.TableQuery

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.bitcoins.chain.config.ChainAppConfig

/**
  * Responsible for creating and destroying database
  * tables inside of the Chain project.
  */
sealed abstract class ChainDbManagement extends DbManagement {

  private val chainTable: TableQuery[BlockHeaderTable] =
    TableQuery[BlockHeaderTable]

  private val filterHeaderTable: TableQuery[CompactFilterHeaderTable] =
    TableQuery[CompactFilterHeaderTable]

  override val allTables = List(chainTable, filterHeaderTable)

  def createHeaderTable(createIfNotExists: Boolean = true)(
      implicit config: ChainAppConfig,
      ec: ExecutionContext): Future[Unit] = {
    createTable(chainTable, createIfNotExists)
  }

  def dropHeaderTable()(implicit config: ChainAppConfig): Future[Unit] = {
    dropTable(chainTable)
  }

  def creareFilterHeaderTable(createIfNotExists: Boolean = true)(
    implicit config: ChainAppConfig,
    ec: ExecutionContext): Future[Unit] = {
    createTable(filterHeaderTable, createIfNotExists)
  }

  def dropFilterHeaderTable()(implicit config: ChainAppConfig): Future[Unit] = {
    dropTable(filterHeaderTable)
  }
}

object ChainDbManagement extends ChainDbManagement
