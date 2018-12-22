package org.bitcoins.node.models

import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.db.DbConfig
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

/**
  * Created by chris on 9/8/16.
  * This is an abstract actor that can be used to implement any sort of
  * actor that accesses a Postgres database. It creates
  * read, update, upsert, and delete methods for your actor to call.
  * You are responsible for the create function. You also need to specify
  * the table and the database you are connecting to.
  */
trait CRUD[T, PrimaryKeyType] extends BitcoinSLogger {
  implicit def ec: ExecutionContext

  /** The table inside our database we are inserting into */
  val table: TableQuery[_ <: Table[T]]

  /** The [[DbConfig]] we used to setup our database connection */
  def dbConfig: DbConfig

  /** Binding to the actual database itself, this is what is used to run querys */
  def database: SafeDatabase = SafeDatabase(dbConfig)

  /**
    * create a record in the database
    *
    * @param t - the record to be inserted
    * @return the inserted record
    */
  def create(t: T): Future[T] = createAll(Vector(t)).map(_.head)

  def createAll(ts: Vector[T]): Future[Vector[T]]

  /**
    * read a record from the database
    *
    * @param id - the id of the record to be read
    * @return Option[T] - the record if found, else none
    */
  def read(id: PrimaryKeyType): Future[Option[T]] = {
    val query = findByPrimaryKey(id)
    val rows: Future[Seq[T]] = database.run(query.result)
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

  /** Updates all of the given ts in the database */
  def updateAll(ts: Vector[T]): Future[Vector[T]] = {
    val query = findAll(ts)
    val actions = ts.map(t => query.update(t))
    val affectedRows: Future[Vector[Int]] = database.run(DBIO.sequence(actions))
    val updatedTs = findAll(ts)
    affectedRows.flatMap { _ =>
      database.runVec(updatedTs.result)
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
    database.run(query.delete)
  }

  /**
    * insert the record if it does not exist, update it if it does
    *
    * @param t - the record to inserted / updated
    * @return t - the record that has been inserted / updated
    */
  def upsert(t: T): Future[T] = upsertAll(Vector(t)).map(_.head)

  /** Upserts all of the given ts in the database, then returns the upserted values */
  def upsertAll(ts: Vector[T]): Future[Vector[T]] = {
    val actions = ts.map(t => table.insertOrUpdate(t))
    val result: Future[Vector[Int]] = database.run(DBIO.sequence(actions))
    val findQueryFuture = result.map(_ => findAll(ts).result)
    findQueryFuture.flatMap(database.runVec(_))
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
                                   ids: Vector[PrimaryKeyType]): Query[Table[_], T, Seq]

  /**
    * return the row that corresponds with this record
    *
    * @param t - the row to find
    * @return query - the sql query to find this record
    */
  protected def find(t: T): Query[Table[_], T, Seq] = findAll(Vector(t))

  protected def findAll(ts: Vector[T]): Query[Table[_], T, Seq]

}

class SafeDatabase(dbConfig: DbConfig) {

  def run[R](a: DBIOAction[R, NoStream, Nothing]): Future[R] = {
    val db = dbConfig.database
    val result = db.run[R](a)
    result
  }

  def runVec[R](a: DBIOAction[Seq[R], NoStream, Nothing])(
    implicit ec: ExecutionContext): Future[Vector[R]] = {
    val db = dbConfig.database
    val result = db.run[Seq[R]](a)
    result.map(_.toVector)
  }
}

object SafeDatabase {
  def apply(db: DbConfig): SafeDatabase = new SafeDatabase(db)
}

case class UpdateFailedException(message: String)
  extends RuntimeException(message)
