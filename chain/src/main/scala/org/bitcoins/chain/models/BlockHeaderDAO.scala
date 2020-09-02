package org.bitcoins.chain.models

import org.bitcoins.chain.blockchain.Blockchain
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.db.BlockHeaderDb
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.db.DatabaseDriver.{PostgreSQL, SQLite}
import org.bitcoins.db._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

/**
  * This class is responsible for all database access related
  * to [[org.bitcoins.core.protocol.blockchain.BlockHeader]]s in
  * our chain project
  */
case class BlockHeaderDAO()(implicit
    ec: ExecutionContext,
    override val appConfig: ChainAppConfig)
    extends CRUD[BlockHeaderDb, DoubleSha256DigestBE]
    with SlickUtil[BlockHeaderDb, DoubleSha256DigestBE] {

  import profile.api._
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers.{doubleSha256DigestBEMapper, int32Mapper, uInt32Mapper}

  implicit private val bigIntMapper: BaseColumnType[BigInt] =
    appConfig.driver match {
      case SQLite     => mappers.bigIntMapper
      case PostgreSQL => mappers.bigIntPostgresMapper
    }

  override val table =
    profile.api.TableQuery[BlockHeaderTable]

  /** Creates all of the given [[BlockHeaderDb]] in the database */
  override def createAll(
      headers: Vector[BlockHeaderDb]): Future[Vector[BlockHeaderDb]] = {
    createAllNoAutoInc(ts = headers, database = safeDatabase)
  }

  override protected def findAll(ts: Vector[BlockHeaderDb]): Query[
    BlockHeaderTable,
    BlockHeaderDb,
    Seq] = {
    findByPrimaryKeys(ts.map(_.hashBE))
  }

  def findByHash(hash: DoubleSha256DigestBE): Future[Option[BlockHeaderDb]] = {
    val query = findByPrimaryKey(hash).result
    safeDatabase.runVec(query).map(_.headOption)
  }

  override def findByPrimaryKeys(hashes: Vector[DoubleSha256DigestBE]): Query[
    BlockHeaderTable,
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
    safeDatabase.runVec(query)
  }

  def getAtHeightQuery(height: Int): profile.StreamingProfileAction[
    Seq[BlockHeaderDb],
    BlockHeaderDb,
    Effect.Read] = {
    table.filter(_.height === height).result
  }

  /** Retrieves a [[BlockHeaderDb]] with the given chain work */
  def getAtChainWork(work: BigInt): Future[Vector[BlockHeaderDb]] = {
    val query = getAtChainWorkQuery(work)
    safeDatabase.runVec(query)
  }

  def getAtChainWorkQuery(work: BigInt): profile.StreamingProfileAction[
    Seq[BlockHeaderDb],
    BlockHeaderDb,
    Effect.Read] = {
    table.filter(_.chainWork === work).result
  }

  /** Gets Block Headers between (inclusive) start height and stop hash, could be out of order */
  def getBetweenHeightAndHash(
      startHeight: Int,
      stopHash: DoubleSha256DigestBE): Future[Vector[BlockHeaderDb]] = {
    for {
      headerOpt <- findByHash(stopHash)
      res <- headerOpt match {
        case Some(header) =>
          getBetweenHeights(startHeight, header.height)
        case None =>
          Future.successful(Vector.empty)
      }
    } yield {
      res
    }
  }

  /** Gets ancestor block headers starting with the given block hash (inclusive)
    * These headers are guaranteed to be in order and a valid chain.
    */
  def getNAncestors(
      childHash: DoubleSha256DigestBE,
      n: Int): Future[Vector[BlockHeaderDb]] = {
    logger.debug(s"Getting $n ancestors for blockhash=$childHash")
    val headerOptF = findByHash(childHash)
    for {
      headerOpt <- headerOptF
      headers <- headerOpt match {
        case Some(header) =>
          val startHeight = Math.max(header.height - n, 0)
          getBetweenHeights(startHeight, header.height)
        case None =>
          Future.successful(Vector.empty)
      }
    } yield {
      headerOpt
        .map { header =>
          val connectedHeaders =
            Blockchain.connectWalkBackwards(header, headers)
          connectedHeaders.reverse
        }
        .getOrElse(Vector.empty)
    }
  }

  /** Gets Block Headers between (inclusive) from and to, could be out of order */
  def getBetweenHeights(from: Int, to: Int): Future[Vector[BlockHeaderDb]] = {
    val query = getBetweenHeightsQuery(from, to)
    safeDatabase.runVec(query)
  }

  def getBetweenHeightsQuery(
      from: Int,
      to: Int): profile.StreamingProfileAction[
    Seq[BlockHeaderDb],
    BlockHeaderDb,
    Effect.Read] = {
    table.filter(header => header.height >= from && header.height <= to).result
  }

  def findAllBeforeTime(time: UInt32): Future[Vector[BlockHeaderDb]] = {
    val query = table.filter(_.time < time)

    database.run(query.result).map(_.toVector)
  }

  def findClosestToTime(time: UInt32): Future[BlockHeaderDb] = {
    require(time >= UInt32(1231006505),
            s"Time must be after the genesis block (1231006505), got $time")

    val query = table.filter(_.time === time)

    val opt = database.run(query.result).map(_.headOption)

    opt.flatMap {
      case None =>
        findAllBeforeTime(time).map(_.maxBy(_.time))
      case Some(header) =>
        Future.successful(header)
    }
  }

  private val lowestNoWorkQuery: profile.ProfileAction[
    Int,
    NoStream,
    Effect.Read] = {
    val noWork =
      table.filter(h => h.chainWork === BigInt(0) || h.chainWork == null)
    noWork.map(_.height).min.getOrElse(0).result
  }

  def getLowestNoWorkHeight: Future[Int] = {
    val query = lowestNoWorkQuery
    database.run(query)
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

  /** Returns the block height of the block with the most work from our database */
  def bestHeight: Future[Int] = {
    chainTips.map { tips =>
      tips.maxByOption(_.chainWork).map(_.height).getOrElse(0)
    }
  }

  private val maxWorkQuery: profile.ProfileAction[
    BigInt,
    NoStream,
    Effect.Read] = {
    val query = table.map(_.chainWork).max.getOrElse(BigInt(0)).result
    query
  }

  /** Returns the chainTips in our database calculated by max height, not work.
    * This should only be used if the chain work has not been calculated
    */
  def chainTipsByHeight: Future[Vector[BlockHeaderDb]] = {
    logger.debug(s"Getting chain tips from: ${dbConfig.config}")
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

    safeDatabase.runVec(aggregate)
  }

  def chainTips: Future[Vector[BlockHeaderDb]] = {
    logger.debug(s"Getting chain tips from: ${dbConfig.config}")
    val aggregate = {
      maxWorkQuery.flatMap { work =>
        logger.debug(s"Max block work: $work")
        val atChainWork = getAtChainWorkQuery(work)
        atChainWork.map { headers =>
          logger.debug(s"Headers at $work: $headers")
        }
        atChainWork
      }
    }

    safeDatabase.runVec(aggregate)
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
  def getBlockchains()(implicit
      ec: ExecutionContext): Future[Vector[Blockchain]] = {
    val chainTipsF = chainTips
    chainTipsF.flatMap { tips =>
      val nestedFuture: Vector[Future[Vector[Blockchain]]] = tips.map { tip =>
        getBlockchainsFrom(tip)
      }
      Future.sequence(nestedFuture).map(_.flatten)
    }
  }

  /** Retrieves a blockchain with the best tip being the given header */
  def getBlockchainFrom(header: BlockHeaderDb)(implicit
      ec: ExecutionContext): Future[Blockchain] = {
    val diffInterval = appConfig.chain.difficultyChangeInterval
    val height = Math.max(0, header.height - diffInterval)
    val headersF = getBetweenHeights(from = height, to = header.height)
    headersF.map(headers =>
      Blockchain.fromHeaders(headers.sortBy(_.height)(Ordering.Int.reverse)))
  }

  def getBlockchainsFrom(header: BlockHeaderDb)(implicit
      ec: ExecutionContext): Future[Vector[Blockchain]] = {
    val diffInterval = appConfig.chain.difficultyChangeInterval
    val height = Math.max(0, header.height - diffInterval)

    getBlockchainsBetweenHeights(from = height, to = header.height)
  }

  @tailrec
  private def loop(
      chains: Vector[Blockchain],
      allHeaders: Vector[BlockHeaderDb]): Vector[Blockchain] = {
    val usedHeaders = chains.flatMap(_.headers).distinct
    val diff = allHeaders.filter(header =>
      !usedHeaders.exists(_.hashBE == header.hashBE))
    if (diff.isEmpty) {
      chains
    } else {
      val sortedDiff = diff.sortBy(_.height)(Ordering.Int.reverse)

      val newChainHeaders =
        Blockchain.connectWalkBackwards(sortedDiff.head, allHeaders)
      val newChain = Blockchain(
        newChainHeaders.sortBy(_.height)(Ordering.Int.reverse))
      loop(chains :+ newChain, allHeaders)
    }
  }

  /** Retrieves a blockchain with the best tip being the given header */
  def getBlockchainsBetweenHeights(from: Int, to: Int)(implicit
      ec: ExecutionContext): Future[Vector[Blockchain]] = {
    getBetweenHeights(from = from, to = to).map { headers =>
      if (headers.map(_.height).distinct.size == headers.size) {
        Vector(
          Blockchain.fromHeaders(
            headers.sortBy(_.height)(Ordering.Int.reverse)))
      } else {
        val headersByHeight: Vector[(Int, Vector[BlockHeaderDb])] =
          headers.groupBy(_.height).toVector
        val tipsOpt = headersByHeight.maxByOption(_._1).map(_._2)

        tipsOpt match {
          case None =>
            Vector.empty
          case Some(tips) =>
            val chains = tips.map { tip =>
              Blockchain
                .connectWalkBackwards(tip, headers)
                .sortBy(_.height)(Ordering.Int.reverse)
            }
            val init = chains.map(Blockchain(_))

            loop(init, headers).distinct
        }
      }
    }
  }

  /** Retrieves a full blockchain with the best tip being the given header */
  def getFullBlockchainFrom(header: BlockHeaderDb)(implicit
      ec: ExecutionContext): Future[Blockchain] = {
    val headersF = getBetweenHeights(from = 0, to = header.height)
    headersF.map(headers => Blockchain.fromHeaders(headers.reverse))
  }

  /** Finds a [[org.bitcoins.chain.models.BlockHeaderDb block header]] that satisfies the given predicate, else returns None */
  def find(f: BlockHeaderDb => Boolean)(implicit
      ec: ExecutionContext): Future[Option[BlockHeaderDb]] = {
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

  /** A table that stores block headers related to a blockchain */
  class BlockHeaderTable(tag: Tag)
      extends Table[BlockHeaderDb](tag, schemaName, "block_headers") {

    def height = column[Int]("height")

    def hash = column[DoubleSha256DigestBE]("hash", O.PrimaryKey)

    def version = column[Int32]("version")

    def previousBlockHash = column[DoubleSha256DigestBE]("previous_block_hash")

    def merkleRootHash = column[DoubleSha256DigestBE]("merkle_root_hash")

    def time = column[UInt32]("time")

    def nBits = column[UInt32]("n_bits")

    def nonce = column[UInt32]("nonce")

    def hex = column[String]("hex")

    def chainWork: Rep[BigInt] = column[BigInt]("chain_work")

    /** The sql index for searching based on [[height]] */
    def heightIndex = index("block_headers_height_index", height)

    def hashIndex = index("block_headers_hash_index", hash)

    def * = {
      (height,
       hash,
       version,
       previousBlockHash,
       merkleRootHash,
       time,
       nBits,
       nonce,
       hex,
       chainWork).<>(BlockHeaderDb.tupled, BlockHeaderDb.unapply)
    }

  }
}
