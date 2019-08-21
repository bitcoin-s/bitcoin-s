package org.bitcoins.db

import scala.concurrent.Future
import slick.jdbc.SQLiteProfile.api._
import scala.concurrent.ExecutionContext

sealed abstract class SlickUtil {

  /** Creates rows in a database that are not auto incremented */
  def createAllNoAutoInc[T, U <: Table[T]](
      ts: Vector[T],
      database: SafeDatabase,
      table: TableQuery[U])(
      implicit ec: ExecutionContext): Future[Vector[T]] = {
    val actions = (table ++= ts).andThen(DBIO.successful(ts)).transactionally
    val result = database.run(actions)
    result
  }
}

object SlickUtil extends SlickUtil
