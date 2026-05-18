package org.bitcoins.server.grpc

import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.testkit.chain.MockChainApi
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.MockNodeApi
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome

import scala.concurrent.{ExecutionContext, Future}

class NodeGrpcRoutesTest extends BitcoinSFixture with PostgresTestDatabase {
  override type FixtureParam = (NodeRoutesClient, ServerGrpc)

  implicit val ec: ExecutionContext = system.dispatcher

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[(NodeRoutesClient, ServerGrpc)] = () => {
      val tmpDir = FileUtil.tmpDir()
      val port = RpcUtil.randomPort
      val host = "localhost"

      val server = new ServerGrpc(
        tmpDir.toPath,
        host,
        port,
        rpcPassword = "",
        chainApi = MockChainApi,
        network = org.bitcoins.core.config.RegTest,
        startedTorConfigF = Future.unit,
        nodeApi = MockNodeApi
      )
      val clientSettings = org.apache.pekko.grpc.GrpcClientSettings
        .connectToServiceAt(host, port)
        .withTls(false)
      val client = NodeRoutesClient(clientSettings)

      server.start().map(_ => (client, server))
    }

    val destroyF: ((NodeRoutesClient, ServerGrpc)) => Future[Unit] = {
      case (client, server) =>
        for {
          _ <- client.close()
          _ <- server.stop()
        } yield ()
    }

    makeDependentFixture[(NodeRoutesClient, ServerGrpc)](builder, destroyF)(
      test)
  }

  behavior of "NodeGrpcRoutes"

  it must "getconnectioncount" in { case (client, _) =>
    client.getConnectionCount(GetConnectionCountRequest()).map { response =>
      assert(response.count == 0)
    }
  }
}
