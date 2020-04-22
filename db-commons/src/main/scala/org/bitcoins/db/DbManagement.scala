package org.bitcoins.db

import org.bitcoins.core.util.BitcoinSLogger
import org.flywaydb.core.Flyway
import org.flywaydb.core.api.FlywayException

import scala.concurrent.{ExecutionContext, Future}

trait DbManagement extends BitcoinSLogger { _: JdbcProfileComponent =>
  import profile.api._

  def allTables: List[TableQuery[Table[_]]]

  /** Lists all tables in the given database */
  def listTables: Future[Vector[SQLiteTableInfo]] = {
    import DbCommonsColumnMappers._
    val query = sql"SELECT * FROM sqlite_master where type='table'"
      .as[SQLiteTableInfo]
    database.run(query)
  }

  /** Creates all tables in our table list, in one SQL transaction */
  def createAll()(implicit ec: ExecutionContext): Future[Unit] = {
    /*    val tables = allTables.map(_.baseTableRow.tableName).mkString(", ")
    logger.debug(s"Creating tables: $tables")*/

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

    database.run(query).map(_ => logger.debug(s"Created tables"))
  }

  def dropAll()(implicit ec: ExecutionContext): Future[Unit] = {
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
      implicit ec: ExecutionContext): Future[Unit] = {
    val tableName = table.baseTableRow.tableName
    logger.debug(
      s"Creating table $tableName with DB config: ${appConfig.config} ")

    val query = createTableQuery(table, createIfNotExists)
    database.run(query).map(_ => logger.debug(s"Created table $tableName"))
  }

  def dropTable(
      table: TableQuery[Table[_]]
  ): Future[Unit] = {
    val result = database.run(table.schema.dropIfExists)
    result
  }

  /** Executes migrations related to this database
    *
    * @see [[https://flywaydb.org/documentation/api/#programmatic-configuration-java]] */
  def migrate(): Int = {
    val url = jdbcUrl
    val username = ""
    val password = ""
    //appConfig.dbName is for the format 'walletdb.sqlite' or 'nodedb.sqlite' etc
    //we need to remove the '.sqlite' suffix
    val name = dbName.split('.').head.mkString
    val config = Flyway.configure().locations(s"classpath:${name}/migration/")
    val flyway = config.dataSource(url, username, password).load

    try {
      flyway.migrate()
    } catch {
      case err: FlywayException =>
        logger.warn(
          s"Failed to apply first round of migrations, attempting baseline and re-apply",
          err)
        //maybe we have an existing database, so attempt to baseline the existing
        //database and then apply migrations again
        flyway.baseline()
        flyway.migrate()
    }
  }
}
