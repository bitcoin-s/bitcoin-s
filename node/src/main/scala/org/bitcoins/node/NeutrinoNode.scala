package org.bitcoins.node

import java.util.concurrent.{Executor, ExecutorService, Executors}

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.p2p.TypeIdentifier
import org.bitcoins.core.protocol.BlockStamp
import org.bitcoins.core.protocol.script.ScriptPubKey
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

  def rescan(
      scriptPubKeysToWatch: Vector[ScriptPubKey],
      startOpt: Option[BlockStamp] = None,
      endOpt: Option[BlockStamp] = None): Future[Unit] = {
    val ec = ExecutionContext.fromExecutor(
      Executors.newFixedThreadPool(
        Runtime.getRuntime.availableProcessors() * 2))
    for {
      chainApi <- chainApiFromDb()
      blockHashes <- chainApi.getMatchingBlocks(scriptPubKeysToWatch,
                                                startOpt,
                                                endOpt)(ec)
      peerMsgSender <- peerMsgSenderF
      _ <- peerMsgSender.sendGetDataMessage(TypeIdentifier.MsgBlock,
                                            blockHashes.map(_.flip): _*)
    } yield {
      ()
    }
  }

}
