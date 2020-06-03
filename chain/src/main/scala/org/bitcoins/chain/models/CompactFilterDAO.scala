package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.gcs.FilterType
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

case class CompactFilterDAO()(
    implicit ec: ExecutionContext,
    override val appConfig: ChainAppConfig)
    extends CRUD[CompactFilterDb, DoubleSha256DigestBE]
    with SlickUtil[CompactFilterDb, DoubleSha256DigestBE] {
  val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers.{
    byteVectorMapper,
    doubleSha256DigestBEMapper,
    filterTypeMapper
  }
  import profile.api._
  implicit private val bigIntMapper: BaseColumnType[BigInt] =
    if (appConfig.driverName == "postgresql") {
      mappers.bigIntPostgresMapper
    } else {
      mappers.bigIntMapper
    }

  class CompactFilterTable(tag: Tag)
      extends Table[CompactFilterDb](tag, "cfilters") {

    def hash = column[DoubleSha256DigestBE]("hash")

    def filterType = column[FilterType]("filter_type")

    def bytes = column[ByteVector]("bytes")

    def height = column[Int]("height")

    def blockHash = column[DoubleSha256DigestBE]("block_hash", O.PrimaryKey)

    def heightIndex = index("cfilters_height_index", height)

    def hashIndex = index("cfilters_hash_index", hash)

    override def * = {
      (hash, filterType, bytes, height, blockHash) <> (CompactFilterDb.tupled, CompactFilterDb.unapply)
    }
  }

  override val table: profile.api.TableQuery[CompactFilterTable] = {
    TableQuery[CompactFilterTable]
  }

  private lazy val blockHeaderTable: profile.api.TableQuery[
    BlockHeaderDAO#BlockHeaderTable] = {
    BlockHeaderDAO().table
  }

  override def createAll(
      filters: Vector[CompactFilterDb]): Future[Vector[CompactFilterDb]] = {
    createAllNoAutoInc(ts = filters, database = safeDatabase)
  }

  /** Finds the rows that correlate to the given primary keys */
  override protected def findByPrimaryKeys(
      ids: Vector[DoubleSha256DigestBE]): Query[
    Table[CompactFilterDb],
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
    safeDatabase.runVec(query)
  }

  private def getAtHeightQuery(height: Int): profile.StreamingProfileAction[
    Seq[CompactFilterDb],
    CompactFilterDb,
    Effect.Read] = {
    table.filter(_.height === height).result
  }

  /** Returns the maximum block height from our database */
  def maxHeight: Future[Int] = {
    val query = maxHeightQuery
    val result = safeDatabase.run(query)
    result
  }

  private val maxHeightQuery: profile.ProfileAction[
    Int,
    NoStream,
    Effect.Read] = {
    val query = table.map(_.height).max.getOrElse(0).result
    query
  }

  /** Gets filters between (inclusive) from and to, could be out of order */
  def getBetweenHeights(from: Int, to: Int): Future[Vector[CompactFilterDb]] = {
    val query = getBetweenHeightsQuery(from, to)
    safeDatabase.runVec(query)
  }

  private def getBetweenHeightsQuery(
      from: Int,
      to: Int): profile.StreamingProfileAction[
    Seq[CompactFilterDb],
    CompactFilterDb,
    Effect.Read] = {
    table.filter(header => header.height >= from && header.height <= to).result
  }

  def getBestFilter: Future[CompactFilterDb] = {
    val join = table join blockHeaderTable on (_.blockHash === _.hash)
    val query = join.groupBy(_._1).map {
      case (filter, headers) =>
        filter -> headers.map(_._2.chainWork).max
    }
    safeDatabase
      .runVec(query.result)
      .map(_.maxBy(_._2.getOrElse(BigInt(0)))._1)
  }
}
