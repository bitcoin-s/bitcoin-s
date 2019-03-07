package org.bitcoins.chain.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.db.{CRUD, DbConfig, SlickUtil}
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

/**
  * This actor is responsible for all database operations relating to
  * [[BlockHeaderDb]]'s. Currently we store all block headers in a database
  */
sealed abstract class BlockHeaderDAO
    extends CRUD[BlockHeaderDb, DoubleSha256DigestBE] {

  import ChainColumnMappers._

  def chainParams: ChainParams

  override val table: TableQuery[BlockHeaderTable] =
    TableQuery[BlockHeaderTable]

  /** Creates all of the given [[BlockHeaderDb]] in the database */
  override def createAll(
      headers: Vector[BlockHeaderDb]): Future[Vector[BlockHeaderDb]] = {
    SlickUtil.createAllNoAutoInc(ts = headers,
                                 database = database,
                                 table = table)
  }

  override protected def findAll(
      ts: Vector[BlockHeaderDb]): Query[Table[_], BlockHeaderDb, Seq] = {
    findByPrimaryKeys(ts.map(_.hashBE))
  }

  def findByHash(hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] = {
    val query = findByPrimaryKey(hash).result
    database.runVec(query).map(_.headOption)
  }

  override def findByPrimaryKeys(hashes: Vector[DoubleSha256DigestBE]): Query[
    Table[_],
    BlockHeaderDb,
    Seq] = {
    table.filter(_.hash.inSet(hashes))
  }

  /** Retrieves a [[BlockHeaderDb]] at the given height */
  def getAtHeight(height: Long): Future[Vector[BlockHeaderDb]] = {
    val query = getAtHeightQuery(height)
    database.runVec(query)
  }

  def getAtHeightQuery(height: Long): SQLiteProfile.StreamingProfileAction[
    Seq[BlockHeaderDb],
    BlockHeaderDb,
    Effect.Read] = {
    table.filter(_.height === height).result
  }

  /** Returns the maximum block height from our database */
  def maxHeight: Future[Long] = {
    val query = maxHeightQuery
    val result = database.run(query)
    result
  }

  private def maxHeightQuery: SQLiteProfile.ProfileAction[
    Long,
    SQLiteProfile.api.NoStream,
    Effect.Read] = {
    val query = table.map(_.height).max.getOrElse(0L).result
    query
  }

  /** Returns the chainTips in our database. This can be multiple headers if we have
    * competing blockchains (fork) */
  def chainTips: Future[Vector[BlockHeaderDb]] = {
    val aggregate = {
      maxHeightQuery.flatMap { height =>
        getAtHeightQuery(height)
      }
    }

    database.runVec(aggregate)
  }
}

object BlockHeaderDAO {
  private case class BlockHeaderDAOImpl(
      chainParams: ChainParams,
      dbConfig: DbConfig)(override implicit val ec: ExecutionContext)
      extends BlockHeaderDAO

  def apply(chainParams: ChainParams, dbConfig: DbConfig)(
      implicit ec: ExecutionContext): BlockHeaderDAO = {
    BlockHeaderDAOImpl(chainParams, dbConfig)(ec)
  }

}
