package org.bitcoins.chain.models

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.db._
import slick.jdbc.SQLiteProfile
import slick.jdbc.SQLiteProfile.api._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/**
  * This class is responsible for all database access related
  * to [[org.bitcoins.core.protocol.blockchain.BlockHeader]]s in
  * our chain project
  */
case class BlockHeaderDAO()(
    implicit ec: ExecutionContext,
    appConfig: ChainAppConfig)
    extends CRUD[BlockHeaderDb, DoubleSha256DigestBE] {

  import org.bitcoins.db.DbCommonsColumnMappers._

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
      height: Int): Future[Option[BlockHeaderDb]] = {
    /*
     * To avoid making many database reads, we make one database read for all
     * possibly useful block headers.
     */
    val headersF = getBetweenHeights(from = height, to = child.height - 1)

    /*
     * We then bucket sort these headers by height so that any ancestor can be found
     * in linear time assuming a bounded number of contentious tips.
     */
    val headersByHeight: Array[Vector[BlockHeaderDb]] =
      new Array[Vector[BlockHeaderDb]](_length = (child.height - height).toInt)

    /*
     * I believe Array's of Objects are instantiated with null, which is evil,
     * and so we start by giving each element of the array a Vector.empty.
     */
    headersByHeight.indices.foreach(index =>
      headersByHeight(index) = Vector.empty)

    // Bucket sort
    headersF.map { headers =>
      headers.foreach { header =>
        val index = (header.height - height).toInt
        headersByHeight(index) = headersByHeight(index).:+(header)
      }

      // Now that the bucket sort is done, we get rid of mutability
      val groupedByHeightHeaders: List[Vector[BlockHeaderDb]] =
        headersByHeight.toList

      @tailrec
      def loop(
          currentHeader: BlockHeaderDb,
          headersByDescHeight: List[Vector[BlockHeaderDb]]): Option[
        BlockHeaderDb] = {
        if (currentHeader.height == height) {
          Some(currentHeader)
        } else {
          val prevHeaderOpt = headersByDescHeight.headOption.flatMap(
            _.find(_.hashBE == currentHeader.previousBlockHashBE))

          prevHeaderOpt match {
            case None             => None
            case Some(prevHeader) => loop(prevHeader, headersByDescHeight.tail)
          }
        }
      }

      loop(child, groupedByHeightHeaders.reverse)
    }
  }

  /** Retrieves a [[BlockHeaderDb]] at the given height */
  def getAtHeight(height: Int): Future[Vector[BlockHeaderDb]] = {
    val query = getAtHeightQuery(height)
    database.runVec(query)
  }

  def getAtHeightQuery(height: Int): SQLiteProfile.StreamingProfileAction[
    Seq[BlockHeaderDb],
    BlockHeaderDb,
    Effect.Read] = {
    table.filter(_.height === height).result
  }

  /** Gets Block Headers between (inclusive) from and to, could be out of order */
  def getBetweenHeights(from: Int, to: Int): Future[Vector[BlockHeaderDb]] = {
    val query = getBetweenHeightsQuery(from, to)
    database.runVec(query)
  }

  def getBetweenHeightsQuery(
      from: Int,
      to: Int): SQLiteProfile.StreamingProfileAction[
    Seq[BlockHeaderDb],
    BlockHeaderDb,
    Effect.Read] = {
    table.filter(header => header.height >= from && header.height <= to).result
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

  /** Returns the chainTips in our database. This can be multiple headers if we have
    * competing blockchains (fork) */
  def chainTips: Future[Vector[BlockHeaderDb]] = {
    logger.debug(s"Getting chaintips from: ${database.config.dbConfig.config}")
    val aggregate = {
      maxHeightQuery.flatMap { height =>
        logger.debug(s"Max block height: $height")
        val atHeight = getAtHeightQuery(height)
        atHeight.map { headers =>
          logger.debug(s"Headers at $height: $headers")
        }
        atHeight
      }
    }

    database.runVec(aggregate)
  }

  /** Returns competing blockchains that are contained in our BlockHeaderDAO
    * Each chain returns the last [[org.bitcoins.core.protocol.blockchain.ChainParams.difficultyChangeInterval difficutly interval]]
    * block headers as defined by the network we are on. For instance, on bitcoin mainnet this will be 2016 block headers.
    * If no competing tips are found, we only return one [[[org.bitcoins.chain.blockchain.Blockchain Blockchain]], else we
    * return n chains for the number of competing [[chainTips tips]] we have
    * @see [[org.bitcoins.chain.blockchain.Blockchain Blockchain]]
    * @param ec
    * @return
    */
  def getBlockchains()(
      implicit ec: ExecutionContext): Future[Vector[Blockchain]] = {
    val chainTipsF = chainTips
    chainTipsF.flatMap { tips =>
      val nestedFuture: Vector[Future[Blockchain]] = tips.map { tip =>
        getBlockchainFrom(tip)
      }
      Future.sequence(nestedFuture)
    }
  }

  /** Retrieves a blockchain with the best tip being the given header */
  def getBlockchainFrom(header: BlockHeaderDb)(
      implicit ec: ExecutionContext): Future[Blockchain] = {
    val diffInterval = appConfig.chain.difficultyChangeInterval
    val height = Math.max(0, header.height - diffInterval)
    val headersF = getBetweenHeights(from = height, to = header.height)
    headersF.map(headers => Blockchain.fromHeaders(headers.reverse))
  }

  /** Finds a [[org.bitcoins.chain.models.BlockHeaderDb block header]] that satisfies the given predicate, else returns None */
  def find(f: BlockHeaderDb => Boolean)(
      implicit ec: ExecutionContext): Future[Option[BlockHeaderDb]] = {
    val chainsF = getBlockchains()
    chainsF.map { chains =>
      val headersOpt: Vector[Option[BlockHeaderDb]] =
        chains.map(_.headers.find(f))
      //if there are multiple, we just choose the first one for now
      val result = headersOpt.filter(_.isDefined).flatten
      if (result.length > 1) {
        logger.warn(
          s"Discarding other matching headers for predicate headers=${result
            .map(_.hashBE.hex)}")
      }
      result.headOption
    }
  }
}
