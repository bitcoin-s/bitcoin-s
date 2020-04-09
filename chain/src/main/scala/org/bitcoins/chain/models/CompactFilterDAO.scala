package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import slick.jdbc.SQLiteProfile
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
      ids: Vector[DoubleSha256DigestBE]): Query[
    Table[_],
    CompactFilterDb,
    Seq] = {
    table.filter(_.blockHash.inSet(ids))
  }

  override protected def findAll(
      ts: Vector[CompactFilterDb]): Query[Table[_], CompactFilterDb, Seq] = {
    findByPrimaryKeys(ts.map(_.blockHashBE))
  }

  def findByBlockHash(
      hash: DoubleSha256DigestBE): Future[Option[CompactFilterDb]] = {
    read(hash)
  }

  /** Retrieves a [[CompactFilterDb]] at the given height */
  def getAtHeight(height: Int): Future[Vector[CompactFilterDb]] = {
    val query = getAtHeightQuery(height)
    database.runVec(query)
  }

  private def getAtHeightQuery(
      height: Int): SQLiteProfile.StreamingProfileAction[
    Seq[CompactFilterDb],
    CompactFilterDb,
    Effect.Read] = {
    table.filter(_.height === height).result
  }

  /** Returns the maximum block height from our database */
  def maxHeight: Future[Int] = {
    val query = maxHeightQuery
    val result = database.run(query)
    result
  }

  private val maxHeightQuery: SQLiteProfile.ProfileAction[
    Int,
    NoStream,
    Effect.Read] = {
    val query = table.map(_.height).max.getOrElse(0).result
    query
  }

  /** Gets filters between (inclusive) from and to, could be out of order */
  def getBetweenHeights(from: Int, to: Int): Future[Vector[CompactFilterDb]] = {
    val query = getBetweenHeightsQuery(from, to)
    database.runVec(query)
  }

  private def getBetweenHeightsQuery(
      from: Int,
      to: Int): SQLiteProfile.StreamingProfileAction[
    Seq[CompactFilterDb],
    CompactFilterDb,
    Effect.Read] = {
    table.filter(header => header.height >= from && header.height <= to).result
  }

}
