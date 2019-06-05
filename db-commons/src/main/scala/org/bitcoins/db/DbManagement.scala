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

  /** Lists all tables in the given database */
  def listTables(db: SafeDatabase): Future[Vector[SQLiteTableInfo]] =
    listTables(db.config.database)

  def createAll()(
      implicit config: AppConfig,
      ec: ExecutionContext): Future[Unit] = {
    Future.sequence(allTables.map(createTable(_))).map(_ => ())
  }

  def dropAll()(
      implicit config: AppConfig,
      ec: ExecutionContext): Future[Unit] = {
    Future.sequence(allTables.reverse.map(dropTable(_))).map(_ => ())
  }

  def createTable(
      table: TableQuery[_ <: Table[_]],
      createIfNotExists: Boolean = true)(
      implicit config: AppConfig,
      ec: ExecutionContext): Future[Unit] = {
    val tableName = table.baseTableRow.tableName
    logger.debug(
      s"Creating table $tableName with DB config: ${config.dbConfig.config} ")

    import config.database
    val query = if (createIfNotExists) {
      table.schema.createIfNotExists
    } else {
      table.schema.create
    }
    database.run(query).map(_ => logger.debug(s"Created table $tableName"))
  }

  def dropTable(
      table: TableQuery[_ <: Table[_]]
  )(implicit config: AppConfig): Future[Unit] = {
    import config.database
    val result = database.run(table.schema.dropIfExists)
    result
  }
}
