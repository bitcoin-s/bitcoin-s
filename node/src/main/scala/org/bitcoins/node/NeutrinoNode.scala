package org.bitcoins.node

import java.util.concurrent.Executors

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer

import scala.concurrent.{ExecutionContext, Future}

case class NeutrinoNode(
    nodePeer: Peer,
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

  override def start(): Future[Node] = {
    val res = for {
      node <- super.start()
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash
      peerMsgSender <- peerMsgSenderF
      _ <- peerMsgSender.sendGetCompactFilterCheckPointMessage(
        stopHash = bestHash.flip)
    } yield {
      node
    }

    res.failed.foreach(logger.error("Cannot start Neutrino node", _))

    res
  }

  def rescan(
      scriptPubKeysToWatch: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp] = None,
      endOpt: Option[BlockStamp] = None): Future[Unit] = {
    val threadPool =
      Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors() * 2)

    val res = for {
      chainApi <- chainApiFromDb()
      blockHashes <- chainApi.getMatchingBlocks(
        scriptPubKeysToWatch,
        startOpt,
        endOpt)(ExecutionContext.fromExecutor(threadPool))
      res <- downloadBlocks(blockHashes.map(_.flip))
    } yield {
      res
    }

    res.onComplete(_ => threadPool.shutdown())

    res.failed.foreach(logger.error("Cannot rescan", _))

    res
  }

}
