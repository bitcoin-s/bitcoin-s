package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.BlockHeaderDb
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer

import scala.concurrent.Future

case class NeutrinoNode(
    nodePeer: Peer,
    nodeCallbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem)
    extends Node {

  override implicit def system: ActorSystem = actorSystem

  override implicit def nodeAppConfig: NodeAppConfig = nodeConfig

  override implicit def chainAppConfig: ChainAppConfig = chainConfig

  override val peer: Peer = nodePeer

  override val callbacks: SpvNodeCallbacks = nodeCallbacks

  private def computeBlockBatches(
      bestHeader: BlockHeaderDb,
      blockCount: Long,
      startHeight: Long,
      batchSize: Int): Future[Vector[(Int, DoubleSha256Digest)]] = {

    val heights = {
      val hs = startHeight.to(blockCount).by(batchSize)
      if (hs.lastOption.exists(_ <= blockCount))
        hs :+ blockCount.toLong + 1
      else
        hs
    }

    val heightPairs = heights.zip(heights.tail.map(_ - 1L))

    val chainApiF = chainApiFromDb()

    heightPairs.foldLeft(Future.successful(Vector.empty[(Int, DoubleSha256Digest)])) { (accF, pair) =>
      val (start, end) = pair
      for {
        chainApi <- chainApiF
        acc <- accF
        header <- chainApi.getHeadersByHeight(end.toInt)
      } yield {
        acc :+ (start.toInt, header.map(_.hashBE.flip).head)
      }
    }
  }

  private def rescan(
      bestHeader: BlockHeaderDb,
      blockCount: Long,
      startHeight: Long,
      batchSize: Int)(
      f: (Int, DoubleSha256Digest) => Future[Unit]): Future[Unit] = {

    val res = for {
      batches <- computeBlockBatches(bestHeader, blockCount, startHeight, batchSize)
      _ <- batches.foldLeft(Future.unit) { case (fut, (startHeight, stopBlock)) => fut.flatMap(_ => f(startHeight, stopBlock)) }
    } yield {
      ()
    }

    res.failed.foreach(e => logger(nodeAppConfig).error(s"Cannot rescan", e))

    res
  }

  override def onStart(): Future[Unit] = {
    for {
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash
      highestFilterHeaderOpt <- chainApi.getHighestFilterHeader
      highestFilterOpt <- chainApi.getHighestFilter
      highestFilterHeaderHeight = highestFilterHeaderOpt.map(_.height).getOrElse(0)
      highestFilterHeaderHash = highestFilterHeaderOpt.map(_.hashBE.flip).getOrElse(DoubleSha256Digest.empty)
//      highestFilterHeight = highestFilterOpt.map(_.height).getOrElse(0)
//      highestFilterHeaderHeight = 0
      peerMsgSender <- peerMsgSenderF
      _ <- peerMsgSender.sendGetCompactFilterCheckPointMessage(stopHash = bestHash.flip)
      _ = logger.info(s"Requesting compact filter headers from=$highestFilterHeaderHeight to=$bestHash")
      (startHeight, stopHash) <- chainApi.nextCompactFilterHeadersRange(highestFilterHeaderHash)
      _ <- peerMsgSender.sendGetCompactFilterHeadersMessage(startHeight, stopHash)
    } yield ()
  }

}
