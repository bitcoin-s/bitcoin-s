package org.bitcoins.db

import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

abstract class DbManagement {
  def allTables: List[TableQuery[_ <: Table[_]]]

  def createAll(dbConfig: DbConfig)(
      implicit ec: ExecutionContext): Future[List[Unit]] = {
    Future.sequence(allTables.map(createTable(_, dbConfig)))
  }

  def dropAll(dbConfig: DbConfig)(
      implicit ec: ExecutionContext): Future[List[Unit]] = {
    Future.sequence(allTables.reverse.map(dropTable(_, dbConfig)))
  }

  def createTable(table: TableQuery[_ <: Table[_]], dbConfig: DbConfig)(
      implicit ec: ExecutionContext): Future[Unit] = {
    val database = dbConfig.database
    val result = database.run(table.schema.create)
    result
  }

  def dropTable(
      table: TableQuery[_ <: Table[_]],
      dbConfig: DbConfig): Future[Unit] = {
    val database = dbConfig.database
    val result = database.run(table.schema.drop)
    result
  }
}
