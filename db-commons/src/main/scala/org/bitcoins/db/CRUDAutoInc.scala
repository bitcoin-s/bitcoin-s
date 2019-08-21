package org.bitcoins.db

import slick.dbio.Effect.Write
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

abstract class CRUDAutoInc[T <: DbRowAutoInc[T]](
    implicit config: AppConfig,
    ec: ExecutionContext)
    extends CRUD[T, Long] {

  /** The table inside our database we are inserting into */
  override val table: TableQuery[_ <: TableAutoInc[T]]

  override def createAll(ts: Vector[T]): Future[Vector[T]] = {
    val query = table
      .returning(table.map(_.id))
      .into((t, id) => t.copyWithId(id = id))
    val actions: DBIOAction[query.MultiInsertResult, NoStream, Write] =
      query.++=(ts)
    database.runVec(actions)
  }

  override def findByPrimaryKeys(ids: Vector[Long]): Query[Table[_], T, Seq] = {
    table.filter(_.id.inSet(ids))
  }

  override def findAll(ts: Vector[T]): Query[Table[_], T, Seq] = {
    val ids = ts.filter(_.id.isDefined).map(_.id.get)
    findByPrimaryKeys(ids)
  }
}
