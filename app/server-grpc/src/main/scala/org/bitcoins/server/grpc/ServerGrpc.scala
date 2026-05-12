package org.bitcoins.server.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.Http.ServerBinding
import org.apache.pekko.http.scaladsl.model.{HttpRequest, HttpResponse}
import org.apache.pekko.http.scaladsl.server.Directives.{
  authenticateBasic,
  complete,
  extractRequest
}
import org.apache.pekko.http.scaladsl.server.directives.Credentials
import org.apache.pekko.http.scaladsl.server.directives.Credentials.Missing
import org.apache.pekko.Done
import org.bitcoins.core.util.StartStopAsync

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
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
class ServerGrpc(
    datadir: Path,
    host: String,
    port: Int,
    rpcPassword: String = ""
)(implicit system: ActorSystem)
    extends StartStopAsync[Unit] {
  import system.dispatcher
  private val impl = new CommonGrpcRoutes(datadir)
  private val handler = CommonRoutesHandler(impl)

  private val bindingOpt: AtomicReference[Option[ServerBinding]] =
    new AtomicReference(None)

  private def authenticator(credentials: Credentials): Option[Done] = {
    credentials match {
      case p @ Credentials.Provided(_) =>
        if (p.verify(rpcPassword)) Some(Done) else None
      case Missing => None
    }
  }

  private val authedHandler: HttpRequest => Future[HttpResponse] =
    if (rpcPassword.isEmpty) {
      handler
    } else {
      val route =
        authenticateBasic("auth", authenticator) { _ =>
          extractRequest { request =>
            complete(handler(request))
          }
        }
      org.apache.pekko.http.scaladsl.server.Route.toFunction(route)
    }

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
  override def start(): Future[Unit] = {
    val bindingF = Http()
      .newServerAt(host, port)
      .bind(authedHandler)
    bindingF.map { b =>
      bindingOpt.set(Some(b))
      ()
    }
  }

  override def stop(): Future[Unit] = {
    bindingOpt.get() match {
      case Some(binding) =>
        binding
          .unbind()
          .map { _ =>
            bindingOpt.set(None)
          }
      case None => Future.unit
    }
  }
}
