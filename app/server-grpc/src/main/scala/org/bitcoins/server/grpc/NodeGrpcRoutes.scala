package org.bitcoins.server.grpc

import org.bitcoins.core.api.node.NodeApi

import scala.concurrent.{ExecutionContext, Future}

/** gRPC service implementation for the NodeRoutes endpoints.
  *
  * This implements the same functionality as [[org.bitcoins.server.NodeRoutes]]
  * but over gRPC instead of HTTP.
  */
class NodeGrpcRoutes(nodeApi: NodeApi)(implicit ec: ExecutionContext)
    extends NodeRoutes {

  override def getConnectionCount(
      in: GetConnectionCountRequest): Future[GetConnectionCountResponse] = {
    nodeApi.getConnectionCount.map(count =>
      GetConnectionCountResponse(count = count))
  }
}
