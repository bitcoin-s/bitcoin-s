package org.bitcoins.chain.models

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.{CRUD, SlickUtil}

import scala.concurrent.{ExecutionContext, Future}

case class CompactFilterHeaderDAO()(implicit
    ec: ExecutionContext,
    override val appConfig: ChainAppConfig)
    extends CRUD[CompactFilterHeaderDb, DoubleSha256DigestBE]
    with SlickUtil[CompactFilterHeaderDb, DoubleSha256DigestBE] {
  import profile.api._
  val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers.doubleSha256DigestBEMapper

  implicit private val bigIntMapper: BaseColumnType[BigInt] =
    if (appConfig.driverName == "postgresql") {
      mappers.bigIntPostgresMapper
    } else {
      mappers.bigIntMapper
    }

  class CompactFilterHeaderTable(tag: Tag)
      extends Table[CompactFilterHeaderDb](tag, "cfheaders") {

    def hash = column[DoubleSha256DigestBE]("hash", O.PrimaryKey)

    def filterHash = column[DoubleSha256DigestBE]("filter_hash")

    def previousFilterHeader =
      column[DoubleSha256DigestBE]("previous_filter_header")

    def blockHash = column[DoubleSha256DigestBE]("block_hash")

    def height = column[Int]("height")

    def heightIndex = index("cfheaders_height_index", height)

    def blockHashIndex = index("cfheaders_block_hash_index", blockHash)

    override def * = {
      (hash,
       filterHash,
       previousFilterHeader,
       blockHash,
       height) <> (CompactFilterHeaderDb.tupled, CompactFilterHeaderDb.unapply)
    }
  }

  override val table: profile.api.TableQuery[CompactFilterHeaderTable] = {
    TableQuery[CompactFilterHeaderTable]
  }

  private lazy val blockHeaderTable: profile.api.TableQuery[
    BlockHeaderDAO#BlockHeaderTable] = {
    BlockHeaderDAO().table
  }

  override def createAll(filterHeaders: Vector[CompactFilterHeaderDb]): Future[
    Vector[CompactFilterHeaderDb]] = {
    createAllNoAutoInc(ts = filterHeaders, database = safeDatabase)
  }

  /** Finds the rows that correlate to the given primary keys */
  override protected def findByPrimaryKeys(
      ids: Vector[DoubleSha256DigestBE]): Query[
    Table[CompactFilterHeaderDb],
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
    safeDatabase.runVec(query.result).map(_.headOption)
  }

  def findAllByBlockHashes(hashes: Vector[DoubleSha256DigestBE]): Future[
    Vector[CompactFilterHeaderDb]] = {
    val query = table.filter(_.blockHash.inSet(hashes))
    safeDatabase.runVec(query.result)
  }

  /** Retrieves a [[CompactFilterHeaderDb]] at the given height */
  def getAtHeight(height: Int): Future[Vector[CompactFilterHeaderDb]] = {
    val query = getAtHeightQuery(height)
    safeDatabase.runVec(query)
  }

  private def getAtHeightQuery(height: Int): slick.sql.FixedSqlStreamingAction[
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

  private val maxHeightQuery: profile.ProfileAction[
    Int,
    NoStream,
    Effect.Read] = {
    val query = table.map(_.height).max.getOrElse(0).result
    query
  }

  def getBestFilterHeader: Future[Option[CompactFilterHeaderDb]] = {
    val join = table
      .join(blockHeaderTable)
      .on(_.blockHash === _.hash)

    val maxQuery = join.map(_._2.chainWork).max

    val query = join.filter(_._2.chainWork === maxQuery).take(1).map(_._1)

    for {
      filterOpt <-
        safeDatabase
          .run(query.result)
          .map(_.headOption)
    } yield filterOpt
  }

}
