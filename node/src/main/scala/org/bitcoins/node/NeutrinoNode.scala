package org.bitcoins.node

import java.util.concurrent.Executors

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.models.{
  BlockHeaderDAO,
  CompactFilterDAO,
  CompactFilterHeaderDAO
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockHeight}
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer

import scala.concurrent.{ExecutionContext, Future}

case class NeutrinoNode(
    nodePeer: Peer,
    nodeCallbacks: SpvNodeCallbacks = SpvNodeCallbacks.empty,
    nodeConfig: NodeAppConfig,
    chainConfig: ChainAppConfig,
    actorSystem: ActorSystem)
    extends Node {
  require(
    nodeConfig.isNeutrinoEnabled,
    s"We need our Neutrino mode enabled to be able to construct a Neutrino node!")

  implicit override def system: ActorSystem = actorSystem

  implicit override def nodeAppConfig: NodeAppConfig = nodeConfig

  implicit override def chainAppConfig: ChainAppConfig = chainConfig

  override val peer: Peer = nodePeer

  override val callbacks: SpvNodeCallbacks = nodeCallbacks

  override def onStart(): Future[Unit] = {
    {
      implicit val ec =
        ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(110))
      for {
        chainApi <- ChainHandler.fromDatabase(BlockHeaderDAO(),
                                              CompactFilterHeaderDAO(),
                                              CompactFilterDAO())

        res <- scanFilters(chainApi)
      } yield {
        println(s"Done: ${res}")
        ec.shutdown()
      }

    }
    val res = for {
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash
      peerMsgSender <- peerMsgSenderF
      _ <- peerMsgSender.sendGetCompactFilterCheckPointMessage(
        stopHash = bestHash.flip)
    } yield {
      ()
    }

    res.failed.foreach(logger.error("Cannot start Neutrino node", _))

    res
  }

  private def scanFilters(chainApi: ChainApi)(implicit ec: ExecutionContext) = {
//    val address = BitcoinAddress("n1RH2x3b3ah4TGQtgrmNAHfmad9wr8U2QY").get
    val address = BitcoinAddress("3GSPn8q1YBkaJ6mZzeVSoAFGmgmZ7iRCBc").get
    chainApi.getMatchingBlocks(
      Vector(address)
      //,
//      Some(BlockHeight(10)),
//      Some(BlockHeight(9))
    )(ec)
  }

}
