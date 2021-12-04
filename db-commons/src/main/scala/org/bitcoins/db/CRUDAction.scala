package org.bitcoins.db

import scala.concurrent.ExecutionContext

abstract class CRUDAction[T, PrimaryKeyType](implicit
    val ec: ExecutionContext,
    override val appConfig: DbAppConfig)
    extends JdbcProfileComponent[DbAppConfig] {
  import profile.api._

  /** The table inside our database we are inserting into */
  val table: profile.api.TableQuery[_ <: profile.api.Table[T]]

  def createAllAction(
      ts: Vector[T]): DBIOAction[Vector[T], NoStream, Effect.Write]

  def createAction(t: T): DBIOAction[T, NoStream, Effect.Write] = {
    createAllAction(Vector(t))
      .map(_.head)
  }

  def updateAction(t: T): DBIOAction[T, NoStream, Effect.Write] = {
    updateAllAction(Vector(t)).map { ts =>
      ts.headOption match {
        case Some(updated) => updated
        case None          => throw UpdateFailedException("Update failed for: " + t)
      }
    }
  }

  protected def find(t: T): Query[Table[_], T, Seq] = findAll(Vector(t))

  protected def findAll(ts: Vector[T]): Query[Table[_], T, Seq]

  /** Updates all of the given ts.
    * Returns all ts that actually existed in the database and got updated
    * This method discards things that did not exist in the database,
    * thus could not be updated
    */
  def updateAllAction(
      ts: Vector[T]): DBIOAction[Vector[T], NoStream, Effect.Write] = {
    val updateActions: Vector[DBIOAction[Option[T], NoStream, Effect.Write]] = {
      ts.map { t =>
        find(t).update(t).flatMap { rowsUpdated =>
          if (rowsUpdated == 0) {
            DBIO.successful(None)
          } else if (rowsUpdated == 1) {
            DBIO.successful(Some(t))
          } else {
            DBIO.failed(new RuntimeException(
              s"Updated more rows that we intended to update, updated=$rowsUpdated"))
          }
        }

      }
    }
    val sequencedA: DBIOAction[Vector[Option[T]], NoStream, Effect.Write] = {
      DBIO.sequence(updateActions)
    }

    //discard all rows that did not exist,
    //thus cannot be updated
    sequencedA.map(_.flatten)
  }

  def deleteAction(t: T): DBIOAction[Int, NoStream, Effect.Write] = {
    deleteAllAction(Vector(t))
  }

  def deleteAllAction(
      ts: Vector[T]): DBIOAction[Int, NoStream, Effect.Write] = {
    val query = findAll(ts)
    query.delete
  }

  /** WARNING: Deletes all rows in table, use with care */
  def deleteAllAction(): DBIOAction[
    Int,
    NoStream,
    Effect.Write with Effect.Transactional] = {
    table.delete.transactionally
  }

}
