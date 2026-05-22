package org.bitcoins.server.grpc

import org.bitcoins.testkit.fixtures.ServerGrpcFixture

import org.scalatest.FutureOutcome

class NodeGrpcRoutesTest extends ServerGrpcFixture {
  override type GrpcClient = NodeRoutesClient
  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withNodeRoutesClient(test)
  }

  behavior of "NodeGrpcRoutes"

  it must "getconnectioncount" in { case clientServer =>
    val client = clientServer.client
    client.getConnectionCount(GetConnectionCountRequest()).map { response =>
      assert(response.count == 1)
    }
  }
}
