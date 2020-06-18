package org.bitcoins.db

import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait SlickUtil[T, PrimaryKeyType] { _: CRUD[T, PrimaryKeyType] =>
  def profile: JdbcProfile

  import profile.api._

  /** Creates rows in a database that are not auto incremented */
  def createAllNoAutoInc(ts: Vector[T], database: SafeDatabase)(implicit
      ec: ExecutionContext): Future[Vector[T]] = {
    val actions = (table ++= ts).andThen(DBIO.successful(ts)).transactionally
    val result = database.run(actions)
    result
  }
}
