package org.bitcoins.node.models

import slick.jdbc.SQLiteProfile.api._
import org.bitcoins.db.CRUD
import org.bitcoins.node.config.NodeAppConfig
import scala.concurrent.ExecutionContext
import org.bitcoins.core.crypto.ECPublicKey
import scala.concurrent.Future

case class InterestingPubKeyDAO()(
    implicit val appConfig: NodeAppConfig,
    val ec: ExecutionContext)
    extends CRUD[ECPublicKey, ECPublicKey] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  def createAll(ts: Vector[ECPublicKey]): Future[Vector[ECPublicKey]] = {
    val actions = ts.map(table += _)
    database.run(DBIO.sequence(actions)).map(_ => ts)
  }

  protected def findAll(
      ts: Vector[ECPublicKey]): Query[Table[_], ECPublicKey, Seq] = ???

  protected def findByPrimaryKeys(
      ids: Vector[ECPublicKey]): Query[Table[_], ECPublicKey, Seq] = ???

  val table = TableQuery[InterestingPubKeyTable]
}
