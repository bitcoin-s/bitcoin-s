package org.bitcoins.server.grpc

import org.bitcoins.core.util.EnvUtil
import org.bitcoins.db.DatadirUtil

import java.io.File
import java.nio.file.Path
import scala.concurrent.Future
import scala.util.{Failure, Success}

/** gRPC service implementation for the CommonRoutes endpoints.
  *
  * This implements the same functionality as
  * [[org.bitcoins.server.routes.CommonRoutes]] but over gRPC instead of HTTP.
  */
class CommonGrpcRoutes(datadir: Path) extends CommonRoutes {

  override def getVersion(in: GetVersionRequest): Future[GetVersionResponse] = {
    val versionOpt = Option(EnvUtil.getVersion)
    Future.successful(GetVersionResponse(version = versionOpt))
  }

  override def zipDataDir(in: ZipDataDirRequest): Future[ZipDataDirResponse] = {
    val path = new File(in.path).toPath
    DatadirUtil.zipDatadir(datadir, path) match {
      case Success(_) => Future.successful(ZipDataDirResponse())
      case Failure(ex) =>
        val status = ex match {
          case _: IllegalArgumentException => io.grpc.Status.INVALID_ARGUMENT
          case _                           => io.grpc.Status.INTERNAL
        }

        Future.failed(
          status
            .withDescription(ex.getMessage)
            .withCause(ex)
            .asRuntimeException()
        )
    }
  }
}
