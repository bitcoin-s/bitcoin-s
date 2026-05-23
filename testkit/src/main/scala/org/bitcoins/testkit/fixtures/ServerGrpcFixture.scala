package org.bitcoins.testkit.fixtures
import org.apache.pekko.grpc.GrpcClientSettings
import org.apache.pekko.grpc.scaladsl.PekkoGrpcClient
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.node.Node
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.server.grpc.{
  ChainRoutesClient,
  CommonRoutesClient,
  DLCRoutesClient,
  NodeRoutesClient,
  ServerGrpc
}
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.dlc.MockDLCNodeApi
import org.bitcoins.testkit.node.{NodeTestUtil, NodeTestWithCachedBitcoind}
import org.bitcoins.testkit.rpc.CachedBitcoindNewest
import org.scalatest.FutureOutcome

import scala.concurrent.Future

trait ServerGrpcFixture
    extends NodeTestWithCachedBitcoind
    with CachedBitcoindNewest {
  lazy val network: BitcoinNetwork = RegTest
  case class GrpcClientServerFixture[T <: PekkoGrpcClient](
      client: T,
      server: ServerGrpc,
      chainApi: ChainApi,
      nodeApi: Node,
      appConfig: BitcoinSAppConfig)

  type GrpcClient <: PekkoGrpcClient
  final override type FixtureParam = GrpcClientServerFixture[GrpcClient]

  override protected def getFreshConfig: BitcoinSAppConfig = {
    BitcoinSTestAppConfig.getMultiPeerNeutrinoWithEmbeddedDbTestConfig(
      postgresOpt,
      Vector.empty
    )
  }
  private def buildClient[T <: PekkoGrpcClient](
      host: String,
      port: Int,
      build: GrpcClientSettings => T,
      serverPackageF: Future[(ServerGrpc, ChainApi, Node, BitcoinSAppConfig)])
      : Future[GrpcClientServerFixture[T]] = {
    val clientSettings = GrpcClientSettings
      .connectToServiceAt(host, port)
      .withTls(false)
    val client: T = build(clientSettings)
    for {
      (server, chainApi, nodeApi, appConfig) <- serverPackageF
      _ <- server.start()
    } yield GrpcClientServerFixture(client,
                                    server,
                                    chainApi,
                                    nodeApi,
                                    appConfig)
  }

  private def buildGrpcServer(
      tmpDir: java.io.File,
      host: String,
      port: Int,
      rpcPassword: String = "")(implicit appConfig: BitcoinSAppConfig)
      : Future[(ServerGrpc, ChainApi, Node, BitcoinSAppConfig)] = {
    val dlcNode = MockDLCNodeApi.fresh()
    val neutrinoNodeF = cachedBitcoindWithFundsF
      .flatMap(createNeutrinoNodeConnectedToBitcoindCached(_)(appConfig))
    for {
      neutrinoNode <- neutrinoNodeF
      chainApi <- neutrinoNode.node.chainApiFromDb()
      server = new ServerGrpc(
        tmpDir.toPath,
        host,
        port,
        rpcPassword = rpcPassword,
        chainApi = chainApi,
        network = network,
        startedTorConfigF = Future.unit,
        nodeApiF = neutrinoNodeF.map(_.node),
        dlcNodeF = Future.successful(dlcNode)
      )
      bitcoind <- cachedBitcoindWithFundsF
      _ <- NodeTestUtil.awaitAllSync(neutrinoNode.node, bitcoind)
    } yield (server, chainApi, neutrinoNode.node, appConfig)
  }

  def withCommonRoutesClient(test: OneArgAsyncTest): FutureOutcome = {
    val port = RpcUtil.randomPort
    val host = "localhost"
    val config = getFreshConfig
    val serverPackageF =
      buildGrpcServer(FileUtil.tmpDir(), host, port)(config)
    val build: () => Future[GrpcClientServerFixture[CommonRoutesClient]] =
      () => {
        buildClient(host, port, CommonRoutesClient.apply, serverPackageF)
      }

    makeDependentFixture[GrpcClientServerFixture[CommonRoutesClient]](
      build,
      destroyClientServer)(test)
  }

  def withChainRoutesClient(test: OneArgAsyncTest): FutureOutcome = {
    val port = RpcUtil.randomPort
    val host = "localhost"
    val serverPackageF =
      buildGrpcServer(FileUtil.tmpDir(), host, port)(getFreshConfig)
    val build: () => Future[GrpcClientServerFixture[ChainRoutesClient]] =
      () => buildClient(host, port, ChainRoutesClient.apply, serverPackageF)
    makeDependentFixture[GrpcClientServerFixture[ChainRoutesClient]](
      build,
      destroyClientServer)(test)
  }

  def withNodeRoutesClient(test: OneArgAsyncTest): FutureOutcome = {
    val port = RpcUtil.randomPort
    val host = "localhost"
    val serverPackageF =
      buildGrpcServer(FileUtil.tmpDir(), host, port)(getFreshConfig)
    val build: () => Future[GrpcClientServerFixture[NodeRoutesClient]] =
      () => buildClient(host, port, NodeRoutesClient.apply, serverPackageF)

    makeDependentFixture[GrpcClientServerFixture[NodeRoutesClient]](
      build,
      destroyClientServer)(test)
  }

  def withDLCRoutesClient(test: OneArgAsyncTest): FutureOutcome = {
    val port = RpcUtil.randomPort
    val host = "localhost"
    val serverPackageF =
      buildGrpcServer(FileUtil.tmpDir(), host, port)(getFreshConfig)
    val build: () => Future[GrpcClientServerFixture[DLCRoutesClient]] =
      () => buildClient(host, port, DLCRoutesClient.apply, serverPackageF)

    makeDependentFixture[GrpcClientServerFixture[DLCRoutesClient]](
      build,
      destroyClientServer)(test)
  }

  private def destroyClientServer[T <: PekkoGrpcClient](
      cs: GrpcClientServerFixture[T]): Future[Unit] = {
    val (client, server) = (cs.client, cs.server)
    val node = cs.nodeApi
    for {
      _ <- client.close()
      _ <- server.stop()
      _ <- tearDownNode(node, cs.appConfig)
      _ <- cs.appConfig.stop()
    } yield ()
  }
}
