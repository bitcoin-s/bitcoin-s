package org.bitcoins.server.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http

import java.nio.file.Path
import scala.concurrent.Future

/** A gRPC server that exposes the CommonRoutes endpoints.
  *
  * @param datadir
  *   the Bitcoin-S data directory
  * @param host
  *   the host to bind the server on
  * @param port
  *   the port to bind the server on (use 0 for a random available port)
  */
class GrpcServer(
    datadir: Path,
    host: String,
    port: Int
)(implicit system: ActorSystem) {

  private val impl = new CommonGrpcRoutes(datadir)
  private val handler = CommonRoutesHandler(impl)

  /** Starts the gRPC server and returns the server binding. */
  def start(): Future[Http.ServerBinding] = {
    Http()
      .newServerAt(host, port)
      .bind(handler)
  }
}
