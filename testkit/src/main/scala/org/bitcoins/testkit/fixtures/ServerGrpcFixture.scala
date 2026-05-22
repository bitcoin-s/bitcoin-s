package org.bitcoins.testkit.fixtures
import org.apache.pekko.grpc.GrpcClientSettings
import org.apache.pekko.grpc.scaladsl.PekkoGrpcClient
import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.grpc.{
  ChainRoutesClient,
  CommonRoutesClient,
  DLCRoutesClient,
  NodeRoutesClient,
  ServerGrpc
}
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.testkit.chain.MockChainApi
import org.bitcoins.testkit.dlc.MockDLCNodeApi
import org.bitcoins.testkit.node.MockNodeApi
import org.scalatest.FutureOutcome

import scala.concurrent.Future

trait ServerGrpcFixture extends BitcoinSFixture with PostgresTestDatabase {
  lazy val network: BitcoinNetwork = RegTest
  case class GrpcClientServerFixture[T <: PekkoGrpcClient](
      client: T,
      server: ServerGrpc)

  type GrpcClient <: PekkoGrpcClient
  final override type FixtureParam = GrpcClientServerFixture[GrpcClient]

  private def buildClient[T](
      host: String,
      port: Int,
      build: GrpcClientSettings => T): Future[T] = {
    val clientSettings = GrpcClientSettings
      .connectToServiceAt(host, port)
      .withTls(false)
    val client = build(clientSettings)
    Future.successful(client)
  }

  private def buildGrpcServer(
      tmpDir: java.io.File,
      host: String,
      port: Int,
      rpcPassword: String = ""): ServerGrpc = {
    val dlcNode = MockDLCNodeApi.fresh()
    val server = new ServerGrpc(
      tmpDir.toPath,
      host,
      port,
      rpcPassword = rpcPassword,
      chainApi = MockChainApi,
      network = network,
      startedTorConfigF = Future.unit,
      nodeApiF = Future.successful(MockNodeApi),
      dlcNodeF = Future.successful(dlcNode)
    )
    server
  }

  def withCommonRoutesClient(test: OneArgAsyncTest): FutureOutcome = {
    val port = RpcUtil.randomPort
    val host = "localhost"
    val server = buildGrpcServer(FileUtil.tmpDir(), host, port)
    val build: () => Future[GrpcClientServerFixture[CommonRoutesClient]] =
      () =>
        buildClient(host, port, CommonRoutesClient.apply).flatMap { client =>
          server.start().map(_ => GrpcClientServerFixture(client, server))
        }

    makeDependentFixture[GrpcClientServerFixture[CommonRoutesClient]](
      build,
      destroyClientServer)(test)
  }

  def withChainRoutesClient(test: OneArgAsyncTest): FutureOutcome = {
    val port = RpcUtil.randomPort
    val host = "localhost"
    val server = buildGrpcServer(FileUtil.tmpDir(), host, port)
    val build: () => Future[GrpcClientServerFixture[ChainRoutesClient]] =
      () =>
        buildClient(host, port, ChainRoutesClient.apply).flatMap { client =>
          server.start().map(_ => GrpcClientServerFixture(client, server))
        }

    makeDependentFixture[GrpcClientServerFixture[ChainRoutesClient]](
      build,
      destroyClientServer)(test)
  }

  def withNodeRoutesClient(test: OneArgAsyncTest): FutureOutcome = {
    val port = RpcUtil.randomPort
    val host = "localhost"
    val server = buildGrpcServer(FileUtil.tmpDir(), host, port)
    val build: () => Future[GrpcClientServerFixture[NodeRoutesClient]] =
      () =>
        buildClient(host, port, NodeRoutesClient.apply).flatMap { client =>
          server.start().map(_ => GrpcClientServerFixture(client, server))
        }

    makeDependentFixture[GrpcClientServerFixture[NodeRoutesClient]](
      build,
      destroyClientServer)(test)
  }

  def withDLCRoutesClient(test: OneArgAsyncTest): FutureOutcome = {
    val port = RpcUtil.randomPort
    val host = "localhost"
    val server = buildGrpcServer(FileUtil.tmpDir(), host, port)
    val build: () => Future[GrpcClientServerFixture[DLCRoutesClient]] =
      () =>
        buildClient(host, port, DLCRoutesClient.apply).flatMap { client =>
          server.start().map(_ => GrpcClientServerFixture(client, server))
        }

    makeDependentFixture[GrpcClientServerFixture[DLCRoutesClient]](
      build,
      destroyClientServer)(test)
  }

  private def destroyClientServer[T <: PekkoGrpcClient](
      cs: GrpcClientServerFixture[T]): Future[Unit] = {
    val (client, server) = (cs.client, cs.server)
    for {
      _ <- client.close()
      _ <- server.stop()
    } yield ()
  }
}
