package org.bitcoins.db

import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait SlickUtilAction[T, PrimaryKeyType] { _: CRUDAction[T, PrimaryKeyType] =>

  def profile: JdbcProfile

  import profile.api._

  def createAllAction(
      ts: Vector[T]): DBIOAction[Vector[T], NoStream, Effect.Write] = {
    val fixedSqlAction = table ++= ts

    fixedSqlAction.map(_ => ts)
  }
}

trait SlickUtil[T, PrimaryKeyType] extends SlickUtilAction[T, PrimaryKeyType] {
  _: CRUD[T, PrimaryKeyType] =>
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
