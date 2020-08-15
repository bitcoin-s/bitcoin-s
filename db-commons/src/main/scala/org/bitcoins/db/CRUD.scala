package org.bitcoins.db

import java.sql.SQLException

import org.bitcoins.core.util.{BitcoinSLogger, FutureUtil}
import slick.dbio.{DBIOAction, NoStream}

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by chris on 9/8/16.
  * This is an abstract actor that can be used to implement any sort of
  * actor that accesses a Postgres database. It creates
  * read, update, upsert, and delete methods for your actor to call.
  * You are responsible for the create function. You also need to specify
  * the table and the database you are connecting to.
  */
abstract class CRUD[T, PrimaryKeyType](implicit
    private val ec: ExecutionContext,
    override val appConfig: AppConfig)
    extends JdbcProfileComponent[AppConfig] {

  import profile.api._

  import scala.language.implicitConversions

  /** We need to cast from TableQuery's of internal types (e.g. AddressDAO#AddressTable) to external
    * versions of them (e.g. AddressDAO().table). You'll notice that although the latter is a subtype
    * of the first, this requires a cast since TableQuery is not covariant in its type parameter.
    *
    * However, since Query is covariant in its first type parameter, I believe the cast from
    * TableQuery[T1] to TableQuery[T2] will always be safe so long as T1 is a subtype of T2
    * AND T1#TableElementType is equal to T2#TableElementType.
    *
    * The above conditions are always the case when this is called within DAOs as it is only
    * ever used for things of the form TableQuery[XDAO().table] -> TableQuery[XDAO#XTable].
    */
  implicit protected def tableQuerySafeSubtypeCast[
      SpecificT <: slick.lifted.AbstractTable[_],
      SomeT <: SpecificT](
      tableQuery: TableQuery[SomeT]): TableQuery[SpecificT] = {
    tableQuery.asInstanceOf[TableQuery[SpecificT]]
  }

  /** The table inside our database we are inserting into */
  val table: profile.api.TableQuery[_ <: profile.api.Table[T]]

  /** Binding to the actual database itself, this is what is used to run querys */
  def safeDatabase: SafeDatabase = SafeDatabase(this)

  /**
    * create a record in the database
    *
    * @param t - the record to be inserted
    * @return the inserted record
    */
  def create(t: T): Future[T] = {
    logger.trace(s"Writing $t to DB with config: ${appConfig.config}")
    createAll(Vector(t)).map(_.head)
  }

  def createAll(ts: Vector[T]): Future[Vector[T]]

  /**
    * read a record from the database
    *
    * @param id - the id of the record to be read
    * @return Option[T] - the record if found, else none
    */
  def read(id: PrimaryKeyType): Future[Option[T]] = {
    logger.trace(s"Reading from DB with config: ${appConfig.config}")
    val query = findByPrimaryKey(id)
    val rows: Future[Seq[T]] = safeDatabase.run(query.result)
    rows.map(_.headOption)
  }

  /** Update the corresponding record in the database */
  def update(t: T): Future[T] = {
    updateAll(Vector(t)).map { ts =>
      ts.headOption match {
        case Some(updated) => updated
        case None          => throw UpdateFailedException("Update failed for: " + t)
      }
    }
  }

  // FIXME: This is a temporary fix for https://github.com/bitcoin-s/bitcoin-s/issues/1586
  // This is an inefficient solution that does each update individually
  def updateAll(ts: Vector[T]): Future[Vector[T]] = {
    def oldUpdateAll(ts: Vector[T]): Future[Vector[T]] = {
      val query = findAll(ts)
      val actions = ts.map(query.update)
      for {
        _ <- safeDatabase.runVec(DBIO.sequence(actions).transactionally)
        result <- safeDatabase.runVec(findAll(ts).result)
      } yield result
    }

    FutureUtil.foldLeftAsync(Vector.empty[T], ts) { (accum, t) =>
      oldUpdateAll(Vector(t)).map(accum ++ _)
    }
  }

  /**
    * delete the corresponding record in the database
    *
    * @param t - the record to be deleted
    * @return int - the number of rows affected by the deletion
    */
  def delete(t: T): Future[Int] = {
    logger.debug("Deleting record: " + t)
    val query: Query[Table[_], T, Seq] = find(t)
    safeDatabase.run(query.delete)
  }

  /**
    * delete all records from the table
    */
  def deleteAll(): Future[Int] =
    safeDatabase.run(table.delete.transactionally)

  /**
    * insert the record if it does not exist, update it if it does
    *
    * @param t - the record to inserted / updated
    * @return t - the record that has been inserted / updated
    */
  def upsert(t: T): Future[T] = {
    upsertAll(Vector(t)).flatMap { ts =>
      ts.headOption match {
        case Some(updated) => Future.successful(updated)
        case None =>
          Future.failed(UpsertFailedException("Upsert failed for: " + t))
      }
    }
  }

  /** Upserts all of the given ts in the database, then returns the upserted values */
  def upsertAll(ts: Vector[T]): Future[Vector[T]] = {
    def oldUpsertAll(ts: Vector[T]): Future[Vector[T]] = {
      val actions = ts.map(t => table.insertOrUpdate(t))
      for {
        _ <- safeDatabase.run(DBIO.sequence(actions).transactionally)
        result <- safeDatabase.runVec(findAll(ts).result)
      } yield result

    }

    FutureUtil.foldLeftAsync(Vector.empty[T], ts) { (accum, t) =>
      oldUpsertAll(Vector(t)).map(accum ++ _)
    }
  }

  /**
    * return all rows that have a certain primary key
    *
    * @param id
    * @return Query object corresponding to the selected rows
    */
  protected def findByPrimaryKey(id: PrimaryKeyType): Query[Table[_], T, Seq] =
    findByPrimaryKeys(Vector(id))

  /** Finds the rows that correlate to the given primary keys */
  protected def findByPrimaryKeys(
      ids: Vector[PrimaryKeyType]): Query[Table[T], T, Seq]

  /**
    * return the row that corresponds with this record
    *
    * @param t - the row to find
    * @return query - the sql query to find this record
    */
  protected def find(t: T): Query[Table[_], T, Seq] = findAll(Vector(t))

  protected def findAll(ts: Vector[T]): Query[Table[_], T, Seq]

  /** Finds all elements in the table */
  def findAll(): Future[Vector[T]] =
    safeDatabase.run(table.result).map(_.toVector)

  /** Returns number of rows in the table */
  def count(): Future[Int] = safeDatabase.run(table.length.result)
}

case class SafeDatabase(jdbcProfile: JdbcProfileComponent[AppConfig])
    extends BitcoinSLogger {

  import jdbcProfile.database
  import jdbcProfile.profile.api.actionBasedSQLInterpolation

  /**
    * SQLite does not enable foreign keys by default. This query is
    * used to enable it. It must be included in all connections to
    * the database.
    */
  private val foreignKeysPragma = sqlu"PRAGMA foreign_keys = TRUE;"
  private val sqlite = jdbcProfile.driverName == "sqlite"

  /** Logs the given action and error, if we are not on mainnet */
  private def logAndThrowError(
      action: DBIOAction[_, NoStream, _]): PartialFunction[
    Throwable,
    Nothing] = {
    case err: SQLException =>
      logger.error(
        s"Error when executing query ${action.getDumpInfo.getNamePlusMainInfo}")
      logger.error(s"$err")
      throw err
  }

  /** Runs the given DB action */
  def run[R](action: DBIOAction[R, NoStream, _])(implicit
      ec: ExecutionContext): Future[R] = {
    val result =
      if (sqlite) database.run[R](foreignKeysPragma >> action)
      else database.run[R](action)
    result.recoverWith { logAndThrowError(action) }
  }

  /**
    * Runs the given DB sequence-returning DB action
    * and converts the result to a vector
    */
  def runVec[R](action: DBIOAction[Seq[R], NoStream, _])(implicit
      ec: ExecutionContext): Future[Vector[R]] = {
    val result = scala.concurrent.blocking {
      if (sqlite) database.run[Seq[R]](foreignKeysPragma >> action)
      else database.run[Seq[R]](action)
    }
    result.map(_.toVector).recoverWith { logAndThrowError(action) }
  }
}

case class UpdateFailedException(message: String)
    extends RuntimeException(message)

case class UpsertFailedException(message: String)
    extends RuntimeException(message)
