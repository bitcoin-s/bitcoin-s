package org.bitcoins.wallet.internal

import java.util.concurrent.Executors

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.wallet.{LockedWallet, WalletLogger}

import scala.concurrent.{ExecutionContext, Future}

private[wallet] trait RescanHandling extends WalletLogger {
  self: LockedWallet =>

  /////////////////////
  // Public facing API

  /** @inheritdoc */
  override def rescanNeutrinoWallet(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int): Future[Unit] = {

    logger.info(s"Starting rescanning the wallet.")

    val res = for {
      _ <- spendingInfoDAO.deleteAll()
      _ <- addressDAO.deleteAll()
      _ <- doNeutrinoRescan(startOpt, endOpt, addressBatchSize)
    } yield ()

    res.onComplete(_ => logger.info("Finished rescanning the wallet"))

    res
  }

  /** @inheritdoc */
  override def rescanSPVWallet(): Future[Unit] =
    Future.failed(new RuntimeException("Rescan not implemented for SPV wallet"))

  /////////////////////
  // Private methods

  private def doNeutrinoRescan(
      startOpt: Option[BlockStamp],
      endOpt: Option[BlockStamp],
      addressBatchSize: Int): Future[Unit] = {
    for {
      scriptPubKeys <- generateScriptPubKeys(addressBatchSize)
      blocks <- matchBlocks(scriptPubKeys = scriptPubKeys,
                            endOpt = endOpt,
                            startOpt = startOpt)
      _ <- downloadAndProcessBlocks(blocks)
      externalGap <- calcAddressGap(HDChainType.External)
      changeGap <- calcAddressGap(HDChainType.Change)
      res <- if (externalGap >= walletConfig.addressGapLimit && changeGap >= walletConfig.addressGapLimit)
        pruneUnusedAddresses()
      else doNeutrinoRescan(startOpt, endOpt, addressBatchSize)
    } yield res
  }

  private def pruneUnusedAddresses(): Future[Unit] = {
    for {
      addressDbs <- addressDAO.findAll()
      _ <- addressDbs.foldLeft(FutureUtil.unit) { (prevF, addressDb) =>
        for {
          _ <- prevF
          spendingInfoDbs <- spendingInfoDAO.findByScriptPubKey(
            addressDb.scriptPubKey)
          _ <- if (spendingInfoDbs.isEmpty) addressDAO.delete(addressDb)
          else FutureUtil.unit
        } yield ()
      }
    } yield ()
  }

  private def calcAddressGap(chainType: HDChainType): Future[Int] = {
    for {
      addressDbs <- addressDAO.findAll()
      addressGap <- addressDbs
      //make sure all addressDb are of the correct chainType
      //and they are sorted according to their index so we can
      //calculate the gap accurately
        .filter(_.path.chain.chainType == chainType)
        .sortBy(_.path.address.index)
        .foldLeft(Future.successful(0)) { (prevNF, addressDb) =>
          for {
            prevN <- prevNF
            spendingInfoDbs <- spendingInfoDAO.findByScriptPubKey(
              addressDb.scriptPubKey)
          } yield {
            if (spendingInfoDbs.isEmpty) prevN + 1 else 0
          }
        }
    } yield {
      logger.debug(s"Address gap: $addressGap")
      addressGap
    }
  }

  private def downloadAndProcessBlocks(
      blocks: Vector[DoubleSha256Digest]): Future[Unit] = {
    logger.debug(s"Requesting ${blocks.size} block(s)")
    blocks.foldLeft(FutureUtil.unit) { (prevF, blockHash) =>
      val completedF = subscribeForBlockProcessingCompletionSignal(blockHash)
      for {
        _ <- prevF
        _ <- nodeApi.downloadBlocks(Vector(blockHash))
        _ <- completedF
      } yield ()
    }
  }

  private def matchBlocks(
      scriptPubKeys: Vector[ScriptPubKey],
      endOpt: Option[BlockStamp],
      startOpt: Option[BlockStamp]): Future[Vector[DoubleSha256Digest]] = {
    val threadPool =
      Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors() * 2)

    val blocksF = for {
      blocks <- getMatchingBlocks(
        scripts = scriptPubKeys,
        startOpt = startOpt,
        endOpt = endOpt)(ExecutionContext.fromExecutor(threadPool))
    } yield {
      blocks.sortBy(_.blockHeight).map(_.blockHash.flip)
    }

    blocksF.onComplete(_ => threadPool.shutdown())
    blocksF
  }

  private def generateScriptPubKeys(
      count: Int): Future[Vector[ScriptPubKey]] = {
    for {
      addresses <- 1
        .to(count)
        .foldLeft(Future.successful(Vector.empty[BitcoinAddress])) {
          (prevFuture, _) =>
            for {
              prev <- prevFuture
              address <- getNewAddress()
            } yield prev :+ address
        }
      changeAddresses <- 1
        .to(count)
        .foldLeft(Future.successful(Vector.empty[BitcoinAddress])) {
          (prevFuture, _) =>
            for {
              prev <- prevFuture
              address <- getNewChangeAddress()
            } yield prev :+ address
        }
    } yield addresses.map(_.scriptPubKey) ++ changeAddresses.map(_.scriptPubKey)
  }

}
