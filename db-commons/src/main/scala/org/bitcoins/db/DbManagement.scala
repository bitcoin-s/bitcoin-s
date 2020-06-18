package org.bitcoins.db

import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import org.flywaydb.core.Flyway
import org.flywaydb.core.api.FlywayException

import scala.concurrent.{ExecutionContext, Future}

trait DbManagement extends BitcoinSLogger {
  _: JdbcProfileComponent[AppConfig] =>
  import profile.api._

  import scala.language.implicitConversions

  /** Internally, slick defines the schema member as
    *
    * def schema: SchemaDescription = buildTableSchemaDescription(q.shaped.value.asInstanceOf[Table[_]])
    *
    * we need to cast between TableQuery's of specific table types to the more generic TableQuery[Table[_]]
    * to get methods in this trait working as they require schema (which essentially does this cast anyway)
    *
    * This cast is needed because TableQuery is not covariant in its type parameter. However, since Query
    * is covariant in its first type parameter, I believe the cast from TableQuery[T1] to TableQuery[T2] will
    * always be safe so long as T1 is a subtype of T2 AND T1#TableElementType is equal to T2#TableElementType.
    *
    * The above conditions are always the case when this is called in the current code base and will
    * stay that way so long as no one tries anything too fancy.
    */
  implicit protected def tableQueryToWithSchema(
      tableQuery: TableQuery[_]): TableQuery[Table[_]] = {
    tableQuery.asInstanceOf[TableQuery[Table[_]]]
  }

  def allTables: List[TableQuery[Table[_]]]

  def dropAll()(implicit ec: ExecutionContext): Future[Unit] = {
    val result =
      FutureUtil
        .foldLeftAsync((), allTables.reverse) { (_, table) =>
          dropTable(table)
        }
    result.failed.foreach { e =>
      e.printStackTrace()
    }
    result
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
      createIfNotExists: Boolean = true)(implicit
      ec: ExecutionContext): Future[Unit] = {
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

  def dropTable(tableName: String): Future[Int] = {
    val result = database.run(sqlu"""DROP TABLE IF EXISTS #$tableName""")
    import scala.concurrent.ExecutionContext.Implicits.global
    result.failed.foreach { ex =>
      ex.printStackTrace()
    }
    result
  }

  /** Executes migrations related to this database
    *
    * @see [[https://flywaydb.org/documentation/api/#programmatic-configuration-java]] */
  def migrate(): Int = {
    val module = appConfig.moduleName
    val config =
      Flyway
        .configure()
        .locations(s"classpath:${driverName}/${module}/migration/")
    val flyway = config.dataSource(jdbcUrl, username, password).load

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
