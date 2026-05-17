package org.bitcoins.testkit.fixtures
import org.apache.pekko.grpc.GrpcClientSettings
import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.grpc.{CommonRoutesClient, ServerGrpc}
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.testkit.chain.MockChainApi
import org.scalatest.FutureOutcome

import scala.concurrent.Future

trait ServerGrpcFixture extends BitcoinSFixture with PostgresTestDatabase {
  lazy val network: BitcoinNetwork = RegTest
  override type FixtureParam = (CommonRoutesClient, ServerGrpc)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[(CommonRoutesClient, ServerGrpc)] = () => {
      val tmpDir = FileUtil.tmpDir()
      val port = RpcUtil.randomPort
      val host = "localhost"
      val server = new ServerGrpc(tmpDir.toPath,
                                  host,
                                  port,
                                  rpcPassword = "",
                                  chainApi = MockChainApi,
                                  network,
                                  Future.unit)
      val clientSettings = GrpcClientSettings
        .connectToServiceAt(host, port)
        .withTls(false)
      val client = CommonRoutesClient(clientSettings)
      server.start().map(_ => (client, server))
    }

    val destroyF: ((CommonRoutesClient, ServerGrpc)) => Future[Unit] = {
      case (client, server) =>
        for {
          _ <- client.close()
          _ <- server.stop()
        } yield ()
    }

    makeDependentFixture[(CommonRoutesClient, ServerGrpc)](builder, destroyF)(
      test)
  }
}
