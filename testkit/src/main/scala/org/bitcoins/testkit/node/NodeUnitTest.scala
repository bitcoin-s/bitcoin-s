package org.bitcoins.testkit.node

import akka.actor.ActorSystem
import org.bitcoins.asyncutil.AsyncUtil
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.api.node.{NodeType, Peer}
import org.bitcoins.node._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.rpc.client.common.BitcoindVersion.V22
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion}
import org.bitcoins.rpc.client.v22.BitcoindV22RpcClient
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.fixture._
import org.bitcoins.testkit.wallet.{BitcoinSWalletTest, WalletWithBitcoindRpc}
import org.bitcoins.wallet.callback.WalletCallbacks
import org.scalatest.FutureOutcome

import java.net.InetSocketAddress
import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

trait NodeUnitTest extends BaseNodeTest {

  def withDisconnectedNeutrinoNode(test: OneArgAsyncTest)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {

    val nodeBuilder: () => Future[NeutrinoNode] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
      for {
        node <- NodeUnitTest.createNeutrinoNode(NodeUnitTest.emptyPeer, None)(
          system,
          appConfig.chainConf,
          appConfig.nodeConf)
        //we aren't calling node.start(), but we need to call appConfig.start()
        //to make sure migrations are run
        _ <- node.chainConfig.start()
        _ <- node.nodeConfig.start()
      } yield node
    }

    makeDependentFixture(
      build = nodeBuilder,
      destroy = (_: Node) => {
        for {
          _ <- ChainUnitTest.destroyAllTables()(appConfig.chainConf,
                                                system.dispatcher)
          _ <- appConfig.stop()
        } yield ()
      }
    )(test)
  }

  def withNeutrinoNodeConnectedToBitcoindV22(test: OneArgAsyncTest)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      NeutrinoNodeConnectedWithBitcoindV22] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
      for {
        bitcoind <-
          BitcoinSFixture
            .createBitcoindWithFunds(Some(V22))
            .map(_.asInstanceOf[BitcoindV22RpcClient])
        peer <- NodeUnitTest.createPeer(bitcoind)
        node <- NodeUnitTest.createNeutrinoNode(peer, None)(system,
                                                            appConfig.chainConf,
                                                            appConfig.nodeConf)
        started <- node.start()
        _ <- NodeUnitTest.syncNeutrinoNode(started, bitcoind)
      } yield NeutrinoNodeConnectedWithBitcoindV22(node, bitcoind)
    }

    makeDependentFixture(
      build = nodeWithBitcoindBuilder,
      destroy = NodeUnitTest.destroyNodeConnectedWithBitcoind(
        _: NodeConnectedWithBitcoind)(system, appConfig)
    )(test)
  }

  def withNeutrinoNodeConnectedToBitcoind(
      test: OneArgAsyncTest,
      versionOpt: Option[BitcoindVersion] = None)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      NeutrinoNodeConnectedWithBitcoind] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
      for {
        bitcoind <- BitcoinSFixture.createBitcoind(versionOpt)
        node <- NodeUnitTest.createNeutrinoNode(bitcoind, None)(
          system,
          appConfig.chainConf,
          appConfig.nodeConf)
        startedNode <- node.start()
        syncedNode <- NodeUnitTest.syncNeutrinoNode(startedNode, bitcoind)
      } yield NeutrinoNodeConnectedWithBitcoind(syncedNode, bitcoind)
    }
    makeDependentFixture(
      build = nodeWithBitcoindBuilder,
      destroy = NodeUnitTest.destroyNodeConnectedWithBitcoind(
        _: NodeConnectedWithBitcoind)(system, appConfig)
    )(test)
  }

  def withUnstartedNeutrinoNodeBitcoinds(
      test: OneArgAsyncTest,
      bitcoinds: Vector[BitcoindRpcClient])(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    val nodeWithBitcoindBuilder: () => Future[
      NeutrinoNodeNotConnectedWithBitcoinds] = { () =>
      require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
      for {
        _ <- appConfig.walletConf.kmConf.start()
        node <- NodeUnitTest.createNeutrinoNode(bitcoinds, None)(
          system,
          appConfig.chainConf,
          appConfig.nodeConf)
      } yield NeutrinoNodeNotConnectedWithBitcoinds(node, bitcoinds)
    }
    makeDependentFixture[NeutrinoNodeNotConnectedWithBitcoinds](
      build = nodeWithBitcoindBuilder,
      destroy =
        NodeUnitTest.destroyNodeNotConnectedWithBitcoinds(_)(system,
                                                             appConfig))(test)
  }

  def withNeutrinoNodeFundedWalletBitcoind(
      test: OneArgAsyncTest,
      versionOpt: Option[BitcoindVersion] = None,
      walletCallbacks: WalletCallbacks = WalletCallbacks.empty)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): FutureOutcome = {
    makeDependentFixture(
      build = () =>
        NodeUnitTest
          .createNeutrinoNodeFundedWalletBitcoind(
            versionOpt = versionOpt,
            walletCallbacks = walletCallbacks)(system, appConfig),
      destroy = NodeUnitTest.destroyNodeFundedWalletBitcoind(
        _: NodeFundedWalletBitcoind)(system, appConfig)
    )(test)
  }
}

object NodeUnitTest extends P2PLogger {

  def buildNode(peer: Peer, walletCreationTimeOpt: Option[Instant])(implicit
      chainConf: ChainAppConfig,
      nodeConf: NodeAppConfig,
      system: ActorSystem): Future[NeutrinoNode] = {
    import system.dispatcher

    val chainConfStartedF = {
      for {
        _ <- chainConf.start()
      } yield {
        ()
      }
    }

    val nodeF =
      chainConfStartedF.map(_ => buildNeutrinoNode(peer, walletCreationTimeOpt))
    for {
      node <- nodeF
      _ <- node.nodeConfig.start()
      startedNode <- node.start()
    } yield {
      startedNode
    }
  }

  private def buildNeutrinoNode(
      peer: Peer,
      walletCreationTimeOpt: Option[Instant])(implicit
      chainConf: ChainAppConfig,
      nodeConf: NodeAppConfig,
      system: ActorSystem): NeutrinoNode = {
    NeutrinoNode(walletCreationTimeOpt,
                 nodeConf,
                 chainConf,
                 system,
                 paramPeers = Vector(peer))
  }

  def destroyNode(node: Node, appConfig: BitcoinSAppConfig)(implicit
      ec: ExecutionContext): Future[Unit] = {

    for {
      _ <- node.stop()
      _ <- node.nodeAppConfig.stop()
      _ <- node.chainAppConfig.stop()
    } yield {
      cleanTables(appConfig)
    }
  }

  def destroyNodeConnectedWithBitcoind(
      nodeConnectedWithBitcoind: NodeConnectedWithBitcoind)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    logger.debug(s"Beginning tear down of node connected with bitcoind")
    import system.dispatcher
    val node = nodeConnectedWithBitcoind.node
    val bitcoind = nodeConnectedWithBitcoind.bitcoind
    val resultF = for {
      _ <- destroyNode(node, appConfig)
      _ <- ChainUnitTest.destroyBitcoind(bitcoind)
      _ <- appConfig.stop()
    } yield {
      logger.debug(s"Done with teardown of node connected with bitcoind!")
      ()
    }

    resultF
  }

  //does not destroys the bitcoinds
  private def destroyNodeConnectedWithBitcoinds(
      nodeConnectedWithBitcoind: NodeConnectedWithBitcoinds)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    logger.debug(s"Beginning tear down of node connected with bitcoind")
    import system.dispatcher
    val node = nodeConnectedWithBitcoind.node
    val resultF = for {
      _ <- destroyNode(node, appConfig)
      _ <- appConfig.stop()
    } yield {
      logger.debug(s"Done with teardown of node connected with bitcoind!")
      ()
    }

    resultF
  }

  private def destroyNodeNotConnectedWithBitcoinds(
      x: NeutrinoNodeNotConnectedWithBitcoinds)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    destroyNodeConnectedWithBitcoinds(
      NeutrinoNodeConnectedWithBitcoinds(x.node, x.bitcoinds))
  }

  /** Creates a neutrino node, a funded bitcoin-s wallet, all of which are connected to bitcoind */
  private def createNeutrinoNodeFundedWalletBitcoind(
      versionOpt: Option[BitcoindVersion],
      walletCallbacks: WalletCallbacks)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[
    NeutrinoNodeFundedWalletBitcoind] = {
    import system.dispatcher
    require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
    for {
      bitcoind <- BitcoinSFixture.createBitcoindWithFunds(versionOpt)
      node <- createNeutrinoNode(bitcoind, None)(system,
                                                 appConfig.chainConf,
                                                 appConfig.nodeConf)
      fundedWallet <- BitcoinSWalletTest.fundedWalletAndBitcoind(
        bitcoindRpcClient = bitcoind,
        nodeApi = node,
        chainQueryApi = node,
        walletCallbacks = walletCallbacks)
      startedNode <- node.start()
      syncedNode <- syncNeutrinoNode(startedNode, bitcoind)
      //callbacks are executed asynchronously, which is how we fund the wallet
      //so we need to wait until the wallet balances are correct
      _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)(
        appConfig.walletConf,
        system)
    } yield {
      NeutrinoNodeFundedWalletBitcoind(node = syncedNode,
                                       wallet = fundedWallet.wallet,
                                       bitcoindRpc = fundedWallet.bitcoind)
    }
  }

  def createNeutrinoNodeFundedWalletFromBitcoind(
      bitcoind: BitcoindRpcClient,
      walletCallbacks: WalletCallbacks)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[
    NeutrinoNodeFundedWalletBitcoind] = {
    import system.dispatcher
    require(appConfig.nodeConf.nodeType == NodeType.NeutrinoNode)
    for {
      _ <- appConfig.walletConf.kmConf.start()
      node <- createNeutrinoNode(bitcoind, None)(system,
                                                 appConfig.chainConf,
                                                 appConfig.nodeConf)

      fundedWallet <- BitcoinSWalletTest.fundedWalletAndBitcoind(
        bitcoindRpcClient = bitcoind,
        nodeApi = node,
        chainQueryApi = bitcoind,
        walletCallbacks = walletCallbacks)

      startedNode <- node.start()
      _ <- NodeTestUtil.awaitConnectionCount(node = node,
                                             expectedConnectionCount = 1)
      //callbacks are executed asynchronously, which is how we fund the wallet
      //so we need to wait until the wallet balances are correct
      _ <- BitcoinSWalletTest.awaitWalletBalances(fundedWallet)(
        appConfig.walletConf,
        system)
    } yield {
      NeutrinoNodeFundedWalletBitcoind(node = startedNode,
                                       wallet = fundedWallet.wallet,
                                       bitcoindRpc = fundedWallet.bitcoind)
    }
  }

  private def destroyNodeFundedWalletBitcoind(
      fundedWalletBitcoind: NodeFundedWalletBitcoind)(implicit
      system: ActorSystem,
      appConfig: BitcoinSAppConfig): Future[Unit] = {
    import system.dispatcher
    val walletWithBitcoind = {
      WalletWithBitcoindRpc(fundedWalletBitcoind.wallet,
                            fundedWalletBitcoind.bitcoindRpc,
                            appConfig.walletConf)
    }

    //these need to be done in order, as the spv node needs to be
    //stopped before the bitcoind node is stopped
    val destroyedF = for {
      _ <- BitcoinSWalletTest.destroyOnlyWalletWithBitcoindCached(
        walletWithBitcoind)
      _ <- destroyNodeConnectedWithBitcoind(
        fundedWalletBitcoind.toNodeConnectedWithBitcoind)
    } yield ()

    destroyedF
  }

  def createPeer(bitcoind: BitcoindRpcClient)(implicit
      executionContext: ExecutionContext): Future[Peer] = {
    NodeTestUtil.getBitcoindPeer(bitcoind)
  }

  def emptyPeer: Peer = {
    val socket = new InetSocketAddress(RpcUtil.randomPort)
    Peer(id = None, socket = socket, socks5ProxyParams = None)
  }

  /** Creates a Neutrino node peered with the given bitcoind client, this does NOT
    * start the neutrino node
    */
  def createNeutrinoNode(
      bitcoind: BitcoindRpcClient,
      walletCreationTimeOpt: Option[Instant])(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[NeutrinoNode] = {
    import system.dispatcher

    val checkConfigF = Future {
      assert(nodeAppConfig.nodeType == NodeType.NeutrinoNode)
    }

    val nodeF = for {
      _ <- checkConfigF
      _ <- chainAppConfig.start()
      _ <- nodeAppConfig.start()
      peer <- createPeer(bitcoind)
    } yield {
      NeutrinoNode(walletCreationTimeOpt,
                   paramPeers = Vector(peer),
                   nodeConfig = nodeAppConfig,
                   chainConfig = chainAppConfig,
                   actorSystem = system)
    }

    nodeF
  }

  /** Creates a Neutrino node peered with the given peer, this does NOT
    * start the neutrino node
    */
  def createNeutrinoNode(peer: Peer, walletCreationTimeOpt: Option[Instant])(
      implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[NeutrinoNode] = {
    import system.dispatcher

    val checkConfigF = Future {
      assert(nodeAppConfig.nodeType == NodeType.NeutrinoNode)
    }

    val nodeF = for {
      _ <- checkConfigF
      _ <- chainAppConfig.start()
      _ <- nodeAppConfig.start()
    } yield {
      NeutrinoNode(walletCreationTimeOpt,
                   paramPeers = Vector(peer),
                   nodeConfig = nodeAppConfig,
                   chainConfig = chainAppConfig,
                   actorSystem = system)
    }

    nodeF
  }

  /** Creates a Neutrino node peered with the given bitcoind client, this does NOT
    * start the neutrino node
    */
  def createNeutrinoNode(
      bitcoinds: Vector[BitcoindRpcClient],
      creationTimeOpt: Option[Instant])(implicit
      system: ActorSystem,
      chainAppConfig: ChainAppConfig,
      nodeAppConfig: NodeAppConfig): Future[NeutrinoNode] = {
    import system.dispatcher

    val checkConfigF = Future {
      assert(nodeAppConfig.nodeType == NodeType.NeutrinoNode)
    }

    val peersF = Future.traverse(bitcoinds)(createPeer(_))
    val nodeF = for {
      _ <- checkConfigF
      _ <- chainAppConfig.start()
      _ <- nodeAppConfig.start()
      peers <- peersF
    } yield {
      NeutrinoNode(walletCreationTimeOpt = creationTimeOpt,
                   paramPeers = peers,
                   nodeConfig = nodeAppConfig,
                   chainConfig = chainAppConfig,
                   actorSystem = system)
    }

    nodeF
  }

  def syncNeutrinoNode(node: NeutrinoNode, bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): Future[NeutrinoNode] = {
    import system.dispatcher
    for {
      syncing <- node.chainApiFromDb().flatMap(_.isSyncing())
      newNode <- {
        if (syncing) {
          //do nothing as we are already syncing
          logger.info(
            s"Node is already syncing, skipping initiating a new sync.")
          NodeTestUtil.awaitSyncAndIBD(node, bitcoind).map(_ => node)
        } else {
          neutrinoNodeSyncHelper(node, bitcoind)
        }
      }
    } yield newNode
  }

  private def neutrinoNodeSyncHelper(
      node: NeutrinoNode,
      bitcoind: BitcoindRpcClient)(implicit
      system: ActorSystem): Future[NeutrinoNode] = {
    import system.dispatcher
    for {
      //wait for bitcoind to be synced internally
      //see: https://github.com/bitcoin/bitcoin/issues/27085
      //see: https://github.com/bitcoin-s/bitcoin-s/issues/4976
      _ <- bitcoind.syncWithValidationInterfaceQueue()
      _ <- node.sync()
      _ <- AsyncUtil.retryUntilSatisfiedF(() =>
        node.chainApiFromDb().flatMap(_.isSyncing()))
    } yield node
  }

  /** This is needed for postgres, we do not drop tables in between individual tests with postgres
    * rather an entire test suite shares the same postgres database.
    * therefore, we need to clean the database after each test, so that migrations can be applied during
    * the setup phase for the next test.
    * @param appConfig
    */
  private def cleanTables(appConfig: BitcoinSAppConfig): Unit = {
    appConfig.nodeConf.clean()
    appConfig.walletConf.clean()
    appConfig.chainConf.clean()
    ()
  }
}
