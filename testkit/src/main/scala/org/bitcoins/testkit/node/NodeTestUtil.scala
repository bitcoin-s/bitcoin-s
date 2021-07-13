package org.bitcoins.testkit.node

import akka.actor.{ActorRefFactory, ActorSystem}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.models.Peer
import org.bitcoins.node.networking.P2PClient
import org.bitcoins.node.networking.peer.PeerMessageReceiver
import org.bitcoins.node.{NeutrinoNode, Node, P2PLogger}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.async.TestAsyncUtil
import org.bitcoins.tor.Socks5ProxyParams

import java.net.InetSocketAddress
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Properties

abstract class NodeTestUtil extends P2PLogger {

  lazy val torEnabled: Boolean = Properties
    .envOrNone("TOR")
    .isDefined

  def client(peer: Peer, peerMsgReceiver: PeerMessageReceiver)(implicit
      ref: ActorRefFactory,
      conf: NodeAppConfig): P2PClient = {
    P2PClient.apply(ref, peer, peerMsgReceiver)
  }

  /** Helper method to get the [[java.net.InetSocketAddress]]
    * we need to connect to to make a p2p connection with bitcoind
    * @param bitcoindRpcClient
    * @return
    */
  def getBitcoindSocketAddress(bitcoindRpcClient: BitcoindRpcClient)(implicit
      executionContext: ExecutionContext): Future[InetSocketAddress] = {
    if (torEnabled) {
      for {
        networkInfo <- bitcoindRpcClient.getNetworkInfo
      } yield {
        val onionAddress = networkInfo.localaddresses
          .find(_.address.endsWith(".onion"))
          .getOrElse(throw new IllegalArgumentException(
            s"bitcoind instance is not configured to use Tor: ${bitcoindRpcClient}"))

        InetSocketAddress.createUnresolved(onionAddress.address,
                                           onionAddress.port)

      }
    } else {
      val instance = bitcoindRpcClient.instance
      Future.successful(
        new InetSocketAddress(instance.uri.getHost, instance.p2pPort))
    }
  }

  def getSocks5ProxyParams: Option[Socks5ProxyParams] = {
    if (torEnabled) {
      Some(
        Socks5ProxyParams(
          address = InetSocketAddress.createUnresolved("127.0.0.1", 9050),
          credentialsOpt = None,
          randomizeCredentials = true
        ))
    } else None
  }

  /** Gets the [[org.bitcoins.node.models.Peer]] that
    * corresponds to [[org.bitcoins.rpc.client.common.BitcoindRpcClient]]
    */
  def getBitcoindPeer(bitcoindRpcClient: BitcoindRpcClient)(implicit
      executionContext: ExecutionContext): Future[Peer] =
    for {
      socket <- getBitcoindSocketAddress(bitcoindRpcClient)
    } yield {
      val socks5ProxyParams = getSocks5ProxyParams
      Peer(socket, socks5ProxyParams = socks5ProxyParams)
    }

  /** Checks if the given node and bitcoind is synced */
  def isSameBestHash(node: Node, rpc: BitcoindRpcClient)(implicit
      ec: ExecutionContext): Future[Boolean] = {
    val hashF = rpc.getBestBlockHash
    for {
      chainApi <- node.chainApiFromDb()
      bestHash <- chainApi.getBestBlockHash()
      hash <- hashF
    } yield {
      bestHash == hash
    }
  }

  def isSameBestFilterHeight(node: NeutrinoNode, rpc: BitcoindRpcClient)(
      implicit ec: ExecutionContext): Future[Boolean] = {
    val rpcCountF = rpc.getBlockCount
    for {
      chainApi <- node.chainApiFromDb()
      filterCount <- chainApi.getFilterCount()
      blockCount <- rpcCountF
    } yield {
      blockCount == filterCount
    }
  }

  def isSameBestFilterHeaderHeight(node: NeutrinoNode, rpc: BitcoindRpcClient)(
      implicit ec: ExecutionContext): Future[Boolean] = {
    val rpcCountF = rpc.getBlockCount
    for {
      chainApi <- node.chainApiFromDb()
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      blockCount <- rpcCountF
    } yield {
      blockCount == filterHeaderCount
    }
  }

  /** Checks if the given light client and bitcoind
    * has the same number of blocks in their blockchains
    */
  def isSameBlockCount(node: Node, rpc: BitcoindRpcClient)(implicit
      ec: ExecutionContext): Future[Boolean] = {
    val rpcCountF = rpc.getBlockCount
    for {
      chainApi <- node.chainApiFromDb()
      count <- chainApi.getBlockCount()
      rpcCount <- rpcCountF
    } yield {
      rpcCount == count
    }
  }

  /** Awaits sync between the given node and bitcoind client */
  def awaitSync(node: Node, rpc: BitcoindRpcClient)(implicit
      sys: ActorSystem): Future[Unit] = {
    import sys.dispatcher
    TestAsyncUtil
      .retryUntilSatisfiedF(() => isSameBestHash(node, rpc),
                            1.second,
                            maxTries = 200)
  }

  /** Awaits sync between the given node and bitcoind client */
  def awaitCompactFilterHeadersSync(node: NeutrinoNode, rpc: BitcoindRpcClient)(
      implicit sys: ActorSystem): Future[Unit] = {
    import sys.dispatcher
    TestAsyncUtil
      .retryUntilSatisfiedF(() => isSameBestFilterHeaderHeight(node, rpc),
                            1.second,
                            maxTries = 200)
  }

  /** Awaits sync between the given node and bitcoind client */
  def awaitCompactFiltersSync(node: NeutrinoNode, rpc: BitcoindRpcClient)(
      implicit sys: ActorSystem): Future[Unit] = {
    import sys.dispatcher
    TestAsyncUtil
      .retryUntilSatisfiedF(() => isSameBestFilterHeight(node, rpc),
                            1.second,
                            maxTries = 200)
  }

  /** The future doesn't complete until the nodes best hash is the given hash */
  def awaitBestHash(hash: DoubleSha256DigestBE, node: Node)(implicit
      system: ActorSystem): Future[Unit] = {
    import system.dispatcher
    def bestHashF: Future[DoubleSha256DigestBE] = {
      node.chainApiFromDb().flatMap(_.getBestBlockHash())
    }
    TestAsyncUtil.retryUntilSatisfiedF(() => bestHashF.map(_ == hash))
  }

}

object NodeTestUtil extends NodeTestUtil
