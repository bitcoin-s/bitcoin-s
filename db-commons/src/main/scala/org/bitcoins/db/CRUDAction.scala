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
}
