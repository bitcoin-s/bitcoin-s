package org.bitcoins.db

import slick.jdbc.JdbcProfile

import scala.concurrent.{ExecutionContext, Future}

trait SlickUtil[T, PrimaryKeyType] { _: CRUD[T, PrimaryKeyType] =>
  def profile: JdbcProfile

  import profile.api._

  /** Creates rows in a database that are not auto incremented */
  def createAllNoAutoInc(ts: Vector[T], database: SafeDatabase)(implicit
      ec: ExecutionContext): Future[Vector[T]] = {
    val actions = (table ++= ts).andThen(DBIO.successful(ts)).transactionally
    database.run(actions).flatMap { created =>
      if (created == ts) {
        Future.successful(created)
      } else {
        Future.failed(new RuntimeException("Upsert failed for: " + ts))
      }
    }
  }
}
