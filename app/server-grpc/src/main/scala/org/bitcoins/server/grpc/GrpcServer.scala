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

  /** Starts the gRPC server and returns the server binding.
    *
    * The returned [[Future]] completes once the server is bound and ready to
    * accept connections. The caller is responsible for managing the server
    * lifecycle by calling [[Http.ServerBinding.unbind]] when the server is no
    * longer needed.
    *
    * Example usage:
    * {{{
    *   val server = new GrpcServer(datadir, "localhost", 8980)
    *   val bindingF = server.start()
    *   // ... handle requests ...
    *   bindingF.flatMap(_.unbind())
    * }}}
    */
  def start(): Future[Http.ServerBinding] = {
    Http()
      .newServerAt(host, port)
      .bind(handler)
  }
}
