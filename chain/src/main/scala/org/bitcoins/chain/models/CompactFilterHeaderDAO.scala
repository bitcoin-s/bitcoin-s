package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import slick.lifted.TableQuery
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class CompactFilterHeaderDAO()(
    implicit ec: ExecutionContext,
    appConfig: ChainAppConfig)
    extends CRUD[CompactFilterHeaderDb, DoubleSha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table = TableQuery[CompactFilterHeaderTable]

  override def createAll(filterHeaders: Vector[CompactFilterHeaderDb]): Future[
    Vector[CompactFilterHeaderDb]] = {
    SlickUtil.createAllNoAutoInc(ts = filterHeaders,
                                 database = database,
                                 table = table)
  }

  /** Finds the rows that correlate to the given primary keys */
  override protected def findByPrimaryKeys(
      ids: Vector[DoubleSha256DigestBE]): SQLiteProfile.api.Query[
    Table[_],
    CompactFilterHeaderDb,
    Seq] = table.filter(_.hash.inSet(ids))

  override protected def findAll(
      ts: Vector[CompactFilterHeaderDb]): SQLiteProfile.api.Query[
    Table[_],
    CompactFilterHeaderDb,
    Seq] = findByPrimaryKeys(ts.map(_.hashBE))

  def findByHash(hash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]] = {
    val query = findByPrimaryKey(hash).result
    database.runVec(query).map(_.headOption)
  }

}
