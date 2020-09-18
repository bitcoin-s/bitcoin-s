package org.bitcoins.chain.blockchain.sync

import org.bitcoins.chain.ChainVerificationLogger
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.api.chain.db.{BlockHeaderDb, CompactFilterHeaderDb}
import org.bitcoins.core.gcs.{FilterHeader, GolombFilter}
import org.bitcoins.core.p2p.CompactFilterMessage
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.crypto.DoubleSha256Digest

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

/** A class that is meant to expose and api to sync
  * [[GolombFilter]]s and [[FilterHeader]]s from an external
  * data source. The important thing to implement is
  * {{{
  *    getFilterFunc: BlockHeader => Future[GolombFilter]
  * }}}
  * which will allow us to sync our internal filters against.
  *
  * It should be noted you are entirely trusting the provider
  * of the `getFilterFunc` as you aren't able to validate the result
  * against another peer that as BIP157 specifies
  *
  * @see [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#client-operation]]
  */
abstract class FilterSync extends ChainVerificationLogger {

  def syncFilters(
      chainApi: ChainApi,
      getFilterFunc: BlockHeader => Future[FilterWithHeaderHash],
      batchSize: Int = 25)(implicit
      ec: ExecutionContext,
      chainAppConfig: ChainAppConfig): Future[ChainApi] = {

    val ourBestFilterHeaderOptF = chainApi.getBestFilterHeader()
    val ourBestBlockHeaderF = chainApi.getBestBlockHeader()
    for {
      oursOpt <- ourBestFilterHeaderOptF
      ourBestBlockHeader <- ourBestBlockHeaderF
      syncedChainApi <- {
        syncFiltersToTip(chainApi = chainApi,
                         ourBestHeader = ourBestBlockHeader,
                         ourBestFilterHeaderOpt = oursOpt,
                         getFilterFunc = getFilterFunc,
                         batchSize)
      }
    } yield {
      syncedChainApi
    }
  }

  private case class BlockFilterAggregated(
      filterHeader: FilterHeader,
      filter: GolombFilter,
      blockHeader: BlockHeader)

  /**
    * Syncs our best filter header to our best block hash
    * @param chainApi our current chain state
    * @param ourBestHeader the block header we are going to sync filters up until
    * @param ourBestFilterHeaderOpt the best filter header we have
    * @param getFilterFunc given a block hash it retrieves filter associated with that hash from our external source
    * @param ec
    * @return
    */
  private def syncFiltersToTip(
      chainApi: ChainApi,
      ourBestHeader: BlockHeaderDb,
      ourBestFilterHeaderOpt: Option[CompactFilterHeaderDb],
      getFilterFunc: BlockHeader => Future[FilterWithHeaderHash],
      batchSize: Int)(implicit
      ec: ExecutionContext,
      chainAppConfig: ChainAppConfig): Future[ChainApi] = {
    val firstBlockHash = ourBestFilterHeaderOpt match {
      case None =>
        logger.info(s"Found no filters in the database, syncing from genesis")
        chainAppConfig.chain.genesisHashBE
      case Some(ourBestFilterHeader) =>
        ourBestFilterHeader.blockHashBE
    }
    if (firstBlockHash == ourBestHeader.hashBE) {
      logger.info(
        s"Our filters are synced with our peers filters, both at blockHash=${firstBlockHash.hex}")
      Future.successful(chainApi)
    } else {
      logger.info(
        s"Beginning sync for filters from filterheader=${firstBlockHash.hex} to blockheader=${ourBestHeader.hashBE.hex}")
      //let's fetch all missing filter headers first
      val bestFilterBlockHeaderF =
        chainApi.getHeader(firstBlockHash)

      val headersMissingFiltersF = for {
        bestFilterBlockHeader <- bestFilterBlockHeaderF
        missing <- chainApi.getHeadersBetween(from = bestFilterBlockHeader.get,
                                              to = ourBestHeader)
      } yield {
        missing
      }

      //because filters can be really large, we don't want to process too many
      //at once, so batch them in groups and the process them.
      val groupedHeadersF: Future[Iterator[Vector[BlockHeaderDb]]] = for {
        missing <- headersMissingFiltersF
      } yield missing.grouped(batchSize)

      val init = Future.successful(chainApi)
      for {
        groupedHeaders <- groupedHeadersF
        finalChainApi <- {
          groupedHeaders.foldLeft(init) {
            case (apiF, missingHeaders) =>
              for {
                api <- apiF
                bestFilterOpt <- api.getBestFilterHeader()
                newApi <- {
                  fetchFiltersForHeaderGroup(api,
                                             missingHeaders,
                                             bestFilterOpt,
                                             getFilterFunc)
                }
              } yield newApi
          }
        }
      } yield finalChainApi
    }
  }

  private def fetchFiltersForHeaderGroup(
      chainApi: ChainApi,
      missingHeaders: Vector[BlockHeaderDb],
      ourBestFilterHeaderOpt: Option[CompactFilterHeaderDb],
      getFilterFunc: BlockHeader => Future[FilterWithHeaderHash])(implicit
      ec: ExecutionContext): Future[ChainApi] = {
    //now that we have headers that are missing filters, let's fetch the filters

    val fetchNested = missingHeaders.map { b =>
      val filterF = getFilterFunc(b.blockHeader)
      filterF.map(f => (b, f))
    }

    val fetchFiltersF: Future[Vector[(BlockHeaderDb, FilterWithHeaderHash)]] = {
      Future.sequence(fetchNested)
    }

    //now let's build filter headers
    val blockFiltersAggF: Future[Vector[BlockFilterAggregated]] = {
      fetchFiltersF.map {
        case filters: Vector[(BlockHeaderDb, FilterWithHeaderHash)] =>
          buildBlockFilterAggregated(filters, ourBestFilterHeaderOpt)
      }
    }

    val compactFiltersF = blockFiltersAggF.map { filtersAgg =>
      filtersAgg.map { agg =>
        CompactFilterMessage(blockHash = agg.blockHeader.hash,
                             filter = agg.filter)
      }
    }

    val blockHeaderOptF = blockFiltersAggF.map { filtersAgg =>
      filtersAgg.lastOption.map(_.blockHeader)
    }
    val filterHeadersF = blockFiltersAggF.map(_.map(_.filterHeader))

    for {
      blockHeaderOpt <- blockHeaderOptF
      compactFilters <- compactFiltersF
      filterHeaders <- filterHeadersF
      filtersChainApi <- {
        blockHeaderOpt match {
          case None =>
            logger.info(
              s"We did not have a block header to process filter headers with! filterHeaders=${filterHeaders} " +
                s"compactFilters=${compactFilters} ourBestFilterHeader=${ourBestFilterHeaderOpt}")
            Future.successful(chainApi)
          case Some(blockHeader) =>
            for {
              headersChainApi <-
                chainApi.processFilterHeaders(filterHeaders, blockHeader.hashBE)
              filtersChainApi <- headersChainApi.processFilters(compactFilters)
            } yield filtersChainApi
        }
      }
    } yield {
      filtersChainApi
    }
  }

  /** This builds a [[BlockFilterAggregated]] data structure
    * and verifies that the filter header hash from an external
    * data source matches the hash of the header we generated internally.
    * If the hash does not match, someone is likely feeding you a bad header chain.
    */
  private def buildBlockFilterAggregated(
      filters: Vector[(BlockHeaderDb, FilterWithHeaderHash)],
      ourBestFilterHeaderOpt: Option[CompactFilterHeaderDb]): Vector[
    BlockFilterAggregated] = {

    val prevFilterHeaderHash = ourBestFilterHeaderOpt match {
      case None                      => DoubleSha256Digest.empty //for the genesis filter
      case Some(ourBestFilterHeader) => ourBestFilterHeader.hashBE.flip
    }
    val accum = new mutable.ArrayBuffer[BlockFilterAggregated](filters.length)

    filters.foreach {
      case (blockHeaderDb, filterWithHash) =>
        val FilterWithHeaderHash(filter, expectedHeaderHash) = filterWithHash
        val filterHeader = if (accum.isEmpty) {
          //first header to connect with our internal headers
          //that have already been validated
          FilterHeader(filterHash = filter.hash,
                       prevHeaderHash = prevFilterHeaderHash)
        } else {
          //get previous filter header's hash
          val prevHeaderHash = accum.last.filterHeader.hash
          FilterHeader(filterHash = filter.hash,
                       prevHeaderHash = prevHeaderHash)
        }

        if (filterHeader.hash == expectedHeaderHash.flip) {
          val agg = BlockFilterAggregated(filterHeader,
                                          filter,
                                          blockHeaderDb.blockHeader)
          accum.append(agg)
        } else {
          sys.error(
            s"The header we created was different from the expected hash we received " +
              s"from an external data source! Something is wrong. Our filterHeader=${filterHeader} " +
              s"expectedHash=$expectedHeaderHash")
        }
    }

    accum.toVector
  }
}

object FilterSync extends FilterSync
