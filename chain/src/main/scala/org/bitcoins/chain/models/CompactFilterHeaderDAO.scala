package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import slick.jdbc.SQLiteProfile
import slick.lifted.TableQuery
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
      ids: Vector[DoubleSha256DigestBE]): Query[
    Table[_],
    CompactFilterHeaderDb,
    Seq] =
    table.filter(_.hash.inSet(ids))

  override protected def findAll(ts: Vector[CompactFilterHeaderDb]): Query[
    Table[_],
    CompactFilterHeaderDb,
    Seq] =
    findByPrimaryKeys(ts.map(_.hashBE))

  def findByHash(
      hash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]] = {
    read(hash)
  }

  def findByBlockHash(
      hash: DoubleSha256DigestBE): Future[Option[CompactFilterHeaderDb]] = {
    val query = table.filter(_.blockHash === hash).take(1)
    database.runVec(query.result).map(_.headOption)
  }

  def findAllByBlockHashes(hashes: Vector[DoubleSha256DigestBE]): Future[
    Vector[CompactFilterHeaderDb]] = {
    val query = table.filter(_.blockHash.inSet(hashes))
    database.runVec(query.result)
  }

  /** Retrieves a [[CompactFilterHeaderDb]] at the given height */
  def getAtHeight(height: Int): Future[Vector[CompactFilterHeaderDb]] = {
    val query = getAtHeightQuery(height)
    database.runVec(query)
  }

  private def getAtHeightQuery(
      height: Int): SQLiteProfile.StreamingProfileAction[
    Seq[CompactFilterHeaderDb],
    CompactFilterHeaderDb,
    Effect.Read] = {
    table.filter(_.height === height).result
  }

  /** Returns the maximum block height from our database */
  def maxHeight: Future[Int] = {
    val query = maxHeightQuery
    val result = database.run(query)
    result
  }

  private def maxHeightQuery: SQLiteProfile.ProfileAction[
    Int,
    NoStream,
    Effect.Read] = {
    val query = table.map(_.height).max.getOrElse(0).result
    query
  }
}
