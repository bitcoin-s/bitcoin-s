package org.bitcoins.db

import org.bitcoins.core.util.BitcoinSLogger
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

abstract class DbManagement extends BitcoinSLogger {
  def allTables: List[TableQuery[_ <: Table[_]]]

  /** Lists all tables in the given database */
  def listTables(db: Database): Future[Vector[SQLiteTableInfo]] = {
    import DbCommonsColumnMappers._
    val query = sql"SELECT * FROM sqlite_master where type='table'"
      .as[SQLiteTableInfo]
    db.run(query)
  }

  def createAll()(
      implicit config: AppConfig,
      ec: ExecutionContext): Future[List[Unit]] = {
    Future.sequence(allTables.map(createTable(_)))
  }

  def dropAll()(
      implicit config: AppConfig,
      ec: ExecutionContext): Future[List[Unit]] = {
    Future.sequence(allTables.reverse.map(dropTable(_)))
  }

  def createTable(
      table: TableQuery[_ <: Table[_]],
      createIfNotExists: Boolean = true)(
      implicit config: AppConfig): Future[Unit] = {
    import config.database
    val result = if (createIfNotExists) {
      database.run(table.schema.createIfNotExists)
    } else {
      database.run(table.schema.create)
    }
    result
  }

  def dropTable(
      table: TableQuery[_ <: Table[_]]
  )(implicit config: AppConfig): Future[Unit] = {
    import config.database
    val result = database.run(table.schema.dropIfExists)
    result
  }
}
