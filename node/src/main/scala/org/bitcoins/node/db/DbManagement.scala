package org.bitcoins.node.db

import org.postgresql.util.PSQLException

import scala.concurrent.{ExecutionContext, Future}
import slick.jdbc.PostgresProfile.api._

abstract class DbManagement {
  def allTables: List[TableQuery[_ <: Table[_]]]

  def createAll(dbConfig: DbConfig)(implicit ec: ExecutionContext): Future[List[Unit]] = {
    Future.sequence(allTables.map(createTable(_, dbConfig)))
  }

  def dropAll(dbConfig: DbConfig)(implicit ec: ExecutionContext): Future[List[Unit]] = {
    Future.sequence(allTables.reverse.map(dropTable(_, dbConfig)))
  }

  def createTable(
                   table: TableQuery[_ <: Table[_]],
                   dbConfig: DbConfig)(implicit ec: ExecutionContext): Future[Unit] = {
    val database = dbConfig.database
    val result = database.run(table.schema.create)
    result.recover {
      case err: PSQLException if err.getMessage.contains("already exists") =>
        ()
    }
  }

  def dropTable(
                 table: TableQuery[_ <: Table[_]],
                 dbConfig: DbConfig): Future[Unit] = {
    val database = dbConfig.database
    val result = database.run(table.schema.drop)
    result
  }
}
