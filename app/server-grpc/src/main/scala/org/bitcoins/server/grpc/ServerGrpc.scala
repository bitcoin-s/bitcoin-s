package org.bitcoins.server.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.Http.ServerBinding
import org.bitcoins.core.util.StartStopAsync

import java.nio.file.Path
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future

class ServerGrpc(
    datadir: Path,
    val host: String,
    val port: Int
)(implicit system: ActorSystem)
    extends StartStopAsync[Unit] {
  import system.dispatcher

  private val impl = new CommonGrpcRoutes(datadir)
  private val handler = CommonRoutesHandler(impl)

  private val bindingOpt: AtomicReference[Option[ServerBinding]] =
    new AtomicReference(None)

  override def start(): Future[Unit] = {
    val bindingF = Http()
      .newServerAt(host, port)
      .bind(handler)

    bindingF.map { binding =>
      bindingOpt.set(Some(binding))
      ()
    }
  }

  override def stop(): Future[Unit] = {
    bindingOpt.get() match {
      case Some(binding) =>
        binding.unbind().map { _ =>
          bindingOpt.set(None)
          ()
        }
      case None => Future.unit
    }
  }
}
