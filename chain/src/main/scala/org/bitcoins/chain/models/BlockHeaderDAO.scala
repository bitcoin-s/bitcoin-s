package org.bitcoins.chain.models

import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.protocol.blockchain.ChainParams
import org.bitcoins.db.{CRUD, DbConfig, SlickUtil}
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

import scala.concurrent.{ExecutionContext, Future}

/**
  * This class is responsible for all database access related
  * to [[org.bitcoins.core.protocol.blockchain.BlockHeader]]s in
  * our chain project
  */
sealed abstract class BlockHeaderDAO
    extends CRUD[BlockHeaderDb, DoubleSha256DigestBE] {

  import org.bitcoins.db.DbCommonsColumnMappers._

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

  /** Retrives the ancestor for the given block header at the given height
    * @param child
    * @param height
    * @return
    */
  def getAncestorAtHeight(
      child: BlockHeaderDb,
      height: Long): Future[Option[BlockHeaderDb]] = {
    //extremely inefficient and probably needs to be re-implemented
    //we are reading from disk every time here
    val headerOptF = read(child.previousBlockHashBE)
    headerOptF.flatMap {
      case Some(header) =>
        if (header.height == height) {
          Future.successful(Some(header))
        } else if (height <= header.height) {
          //need to keep traversing backwards to get to the right height
          getAncestorAtHeight(header, height)
        } else {
          Future.successful(None)
        }
      case None =>
        Future.successful(None)
    }
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
