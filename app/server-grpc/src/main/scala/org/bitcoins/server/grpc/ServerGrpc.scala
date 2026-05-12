package org.bitcoins.server.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.Http.ServerBinding
import org.apache.pekko.http.scaladsl.model.{
  ContentType,
  HttpEntity,
  HttpRequest,
  HttpResponse
}
import org.apache.pekko.http.scaladsl.model.StatusCodes
import org.apache.pekko.http.scaladsl.model.headers.{
  Authorization,
  BasicHttpCredentials,
  RawHeader
}
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.util.StartStopAsync

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.security.MessageDigest
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
    extends StartStopAsync[Unit]
    with BitcoinSLogger {
  import system.dispatcher
  private val impl = new CommonGrpcRoutes(datadir)
  private val handler = CommonRoutesHandler(impl)

  private val bindingOpt: AtomicReference[Option[ServerBinding]] =
    new AtomicReference(None)

  private val grpcStatusHeader = "grpc-status"
  private val grpcMessageHeader = "grpc-message"
  private val grpcContentType: ContentType =
    ContentType.parse("application/grpc") match {
      case Right(contentType) => contentType
      case Left(error) =>
        throw new IllegalArgumentException(
          s"Could not parse gRPC content type: $error")
    }

  private val unauthenticatedResponse =
    HttpResponse(
      status = StatusCodes.OK,
      entity = HttpEntity(grpcContentType, Array.emptyByteArray),
      headers = List(
        RawHeader(grpcStatusHeader,
                  io.grpc.Status.UNAUTHENTICATED.getCode.value().toString),
        RawHeader(grpcMessageHeader, "Unauthenticated")
      )
    )

  private def isAuthenticated(request: HttpRequest): Boolean = {
    request.header[Authorization] match {
      case Some(Authorization(credentials: BasicHttpCredentials)) =>
        passwordsMatch(credentials.password, rpcPassword)
      case _ => false
    }
  }

  private val authedHandler: HttpRequest => Future[HttpResponse] =
    if (rpcPassword.isEmpty) {
      logger.warn(
        s"gRPC authentication is disabled because bitcoin-s.server.password is empty (host=$host, port=$port)"
      )
      handler
    } else {
      request =>
        if (isAuthenticated(request)) {
          handler(request)
        } else {
          Future.successful(unauthenticatedResponse)
        }
    }

  private def passwordsMatch(provided: String, expected: String): Boolean = {
    val providedBytes = provided.getBytes(StandardCharsets.UTF_8)
    val expectedBytes = expected.getBytes(StandardCharsets.UTF_8)
    MessageDigest.isEqual(providedBytes, expectedBytes)
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
