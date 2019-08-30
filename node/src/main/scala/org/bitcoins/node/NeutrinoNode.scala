package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.crypto.DoubleSha256DigestBE
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

  override def onStart(): Future[Unit] = {
    val res = for {
      chainApi <- chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash
      peerMsgSender <- peerMsgSenderF
      _ <- peerMsgSender.sendGetCompactFilterCheckPointMessage(stopHash = bestHash.flip)
    } yield {
      ()
    }

    res.failed.foreach(logger.error("Cannot start Neutrino node", _))

    res
  }

}
