package org.bitcoins.db

import org.flywaydb.core.Flyway
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

abstract class DbManagement extends DatabaseLogger {
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

  /** Creates all tables in our table list, in one SQL transaction */
  def createAll()(
      implicit config: AppConfig,
      ec: ExecutionContext
  ): Future[Unit] = {
    val tables = allTables.map(_.baseTableRow.tableName).mkString(", ")
    logger.debug(s"Creating tables: $tables")

    val query = {
      val querySeq =
        allTables
          .map(createTableQuery(_, createIfNotExists = true))
          .map { query =>
            // DIRTY HACK. For some reason Slick doesn't know that Sqlite can do CREATE INDEX IF NOT EXISTS
            val statements = query.statements.map(
              _.replace("create index", "create index if not exists"))
            query.overrideStatements(statements)
          }
      DBIO.seq(querySeq: _*).transactionally
    }

    import config.database
    database.run(query).map(_ => logger.debug(s"Created tables: $tables"))
  }

  def dropAll()(
      implicit config: AppConfig,
      ec: ExecutionContext): Future[Unit] = {
    Future.sequence(allTables.reverse.map(dropTable(_))).map(_ => ())
  }

  /** The query needed to create the given table */
  private def createTableQuery(
      table: TableQuery[_ <: Table[_]],
      createIfNotExists: Boolean) = {
    if (createIfNotExists) {
      table.schema.createIfNotExists
    } else {
      table.schema.create
    }
  }

  /** Creates the given table */
  def createTable(
      table: TableQuery[_ <: Table[_]],
      createIfNotExists: Boolean = true)(
      implicit config: AppConfig,
      ec: ExecutionContext): Future[Unit] = {
    val tableName = table.baseTableRow.tableName
    logger.debug(
      s"Creating table $tableName with DB config: ${config.dbConfig.config} ")

    import config.database
    val query = createTableQuery(table, createIfNotExists)
    database.run(query).map(_ => logger.debug(s"Created table $tableName"))
  }

  def dropTable(
      table: TableQuery[_ <: Table[_]]
  )(implicit config: AppConfig): Future[Unit] = {
    import config.database
    val result = database.run(table.schema.dropIfExists)
    result
  }

  /** Executes migrations related to this database
    *
    * @see [[https://flywaydb.org/documentation/api/#programmatic-configuration-java]] */
  def migrate(appConfig: AppConfig): Int = {
    val url = appConfig.jdbcUrl
    val username = ""
    val password = ""
    //appConfig.dbName is for the format 'walletdb.sqlite' or 'nodedb.sqlite' etc
    //we need to remove the '.sqlite' suffix
    val dbName = appConfig.dbName.split('.').head.mkString
    val config = Flyway.configure().locations(s"classpath:${dbName}/migration/")
    val flyway = config.dataSource(url, username, password).load
    flyway.migrate()
  }
}
