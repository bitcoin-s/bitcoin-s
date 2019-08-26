package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import slick.lifted.TableQuery
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

case class CompactFilterDAO()(
    implicit ec: ExecutionContext,
    appConfig: ChainAppConfig)
    extends CRUD[CompactFilterDb, DoubleSha256DigestBE] {
  import org.bitcoins.db.DbCommonsColumnMappers._

  override val table = TableQuery[CompactFilterTable]

  override def createAll(
      filters: Vector[CompactFilterDb]): Future[Vector[CompactFilterDb]] = {
    SlickUtil.createAllNoAutoInc(ts = filters,
                                 database = database,
                                 table = table)
  }

  /** Finds the rows that correlate to the given primary keys */
  override protected def findByPrimaryKeys(
      ids: Vector[DoubleSha256DigestBE]): Query[Table[_], CompactFilterDb, Seq] = {
    table.filter(_.hash.inSet(ids))
  }

  override protected def findAll(
      ts: Vector[CompactFilterDb]): Query[Table[_], CompactFilterDb, Seq] = {
    findByPrimaryKeys(ts.map(_.hashBE))
  }

  def findByHash(hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] = {
    read(hash)
  }

  def findByBlockHash(hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] = {
    val query = table.filter(_.blockHash === hash).take(1)
    database.runVec(query.result).map(_.headOption)
  }

  def findHighest(): Future[Option[CompactFilterDb]] = {
    val query = table.filter(_.height === table.map(_.height).max).take(1)
    database.runVec(query.result).map(_.headOption)
  }

}
