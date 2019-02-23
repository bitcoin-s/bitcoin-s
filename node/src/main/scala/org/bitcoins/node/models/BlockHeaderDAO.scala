package org.bitcoins.node.models

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.core.util.BitcoinSUtil
import org.bitcoins.node.constant.Constants
import org.bitcoins.node.db.DbConfig
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}
/**
  * Created by chris on 9/8/16.
  * This actor is responsible for all database operations relating to
  * [[BlockHeader]]'s. Currently we store all block headers in a postgresql database
  */
sealed abstract class BlockHeaderDAO extends CRUD[BlockHeader, DoubleSha256Digest] {
  import ColumnMappers._

  override val table = TableQuery[BlockHeaderTable]

  override def create(blockHeader: BlockHeader): Future[BlockHeader] = {
    val action = {
      if (blockHeader == Constants.chainParams.genesisBlock.blockHeader) {
        //we need to make an exception for the genesis block, it does not have a previous hash
        //so we remove that invariant in this sql statement
        buildGenesisHeaderQuery(blockHeader)
      } else {
        insertStatement(blockHeader)
      }
    }
    database.run(action)
  }

  /** Creates all of the given [[BlockHeader]] in the database */
  override def createAll(headers: Vector[BlockHeader]): Future[Vector[BlockHeader]] = {
    headers.headOption match {
      case Some(header) =>
        val first = {
          if (header == Constants.chainParams.genesisBlock.blockHeader) {
            buildGenesisHeaderQuery(header)
          } else {
            insertStatement(header)
          }
        }
        val rest = headers.tail.map(insertStatement(_))
        val all = first +: rest

        val actions = DBIO.sequence(all)
        database.run(actions)
      case None =>
        Future.successful(Vector.empty)
    }
  }

  /** Special query here to insert a block header with height zero */
  private def buildGenesisHeaderQuery(blockHeader: BlockHeader): DBIOAction[BlockHeader,NoStream,Effect] = {
    require(Constants.chainParams.genesisBlock.blockHeader == blockHeader, s"Header at height zero must be genesis block")
    sqlu"insert into block_headers values(0, ${blockHeader.hash.hex}, ${blockHeader.version.toLong}, ${blockHeader.previousBlockHash.hex}, ${blockHeader.merkleRootHash.hex}, ${blockHeader.time.toLong}, ${blockHeader.nBits.toLong}, ${blockHeader.nonce.toLong}, ${blockHeader.hex})"
      .andThen(DBIO.successful(blockHeader))
  }
  /** This is the custom insert statement needed for block headers, the magic here is it
    * increments the height of the previous [[BlockHeader]] by one.
    * See this question for how the statement works
    * [[http://stackoverflow.com/questions/39628543/derive-column-value-from-another-rows-column-value-slick]]
    * @param blockHeader
    * @return
    */
  private def insertStatement(blockHeader: BlockHeader) = {
    sqlu"insert into block_headers (height, hash, version, previous_block_hash, merkle_root_hash, time,n_bits,nonce,hex) select height + 1, ${blockHeader.hash.hex}, ${blockHeader.version.toLong}, ${blockHeader.previousBlockHash.hex}, ${blockHeader.merkleRootHash.hex}, ${blockHeader.time.toLong}, ${blockHeader.nBits.toLong}, ${blockHeader.nonce.toLong}, ${blockHeader.hex}  from block_headers where hash = ${blockHeader.previousBlockHash.hex}"
      .flatMap { rowsAffected =>
        if (rowsAffected == 0) {
          val exn = new IllegalArgumentException(
            "Failed to insert blockHeader: " + BitcoinSUtil.flipEndianness(
              blockHeader.hash.bytes) + " prevHash: " + BitcoinSUtil
              .flipEndianness(blockHeader.previousBlockHash.bytes))
          DBIO.failed(exn)
        } else DBIO.successful(blockHeader)
      }
  }

  override protected def findAll(ts: Vector[BlockHeader]): Query[Table[_], BlockHeader, Seq] = {
    findByPrimaryKeys(ts.map(_.hash))
  }

  override def findByPrimaryKeys(
      hashes: Vector[DoubleSha256Digest]): Query[Table[_], BlockHeader, Seq] = {
    table.filter(_.hash.inSet(hashes))
  }

  /** Retrieves a [[BlockHeader]] at the given height, if none is found it returns None */
  def getAtHeight(height: Long): Future[(Long, Vector[BlockHeader])] = {
    //which would both have height x
    val query = table.filter(_.height === height).result
    database.run(query).map(h => (height, h.toVector))
  }

  /** Finds the height of the given [[BlockHeader]]'s hash, if it exists */
  def findHeight(
      hash: DoubleSha256Digest): Future[Option[(Long, BlockHeader)]] = {
    import ColumnMappers._
    val query = table.filter(_.hash === hash).map(x => (x.height, x)).result
    val b: Future[Option[(Long, BlockHeader)]] =
      database.run(query).map(_.headOption)
    b
  }

  /** Returns the maximum block height from our database */
  def maxHeight: Future[Long] = {
    val query = table.map(_.height).max.result
    val result = database.run(query)
    result.map(_.getOrElse(0L))
  }

  /** Returns the chainTips in our database. This can be multiple headers if we have
    * competing blockchains (fork) */
  def chainTips: Future[Vector[BlockHeader]] = {
    val maxF = maxHeight

    maxF.flatMap { maxHeight =>
      getAtHeight(maxHeight)
        .map(_._2.toVector)
    }
  }
}

object BlockHeaderDAO {
  private case class BlockHeaderDAOImpl(dbConfig: DbConfig)(override implicit val ec: ExecutionContext)
      extends BlockHeaderDAO

  def apply(dbConfig: DbConfig)(implicit ec: ExecutionContext): BlockHeaderDAO = {
    BlockHeaderDAOImpl(dbConfig)(ec)
  }

}
