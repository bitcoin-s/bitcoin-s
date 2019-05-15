package org.bitcoins.node

import akka.actor.ActorSystem
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.chain.blockchain.ChainHandler
import org.bitcoins.chain.models.BlockHeaderDAO
import org.bitcoins.db.AppConfig
import org.bitcoins.node.networking.peer.PeerMessageSender

case class SpvNode()(implicit appConfig: AppConfig, system: ActorSystem) {
  import system.dispatcher

  private val blockHeaderDAO: BlockHeaderDAO = BlockHeaderDAO(appConfig)

  private val chainApi: ChainApi =
    ChainHandler(blockHeaderDAO = blockHeaderDAO, chainConfig = appConfig)

  private val peerMsgSender: PeerMessageSender = ???
}
