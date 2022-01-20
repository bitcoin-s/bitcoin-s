package org.bitcoins.server.routes

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.db.DatadirUtil

import java.io.File
import java.nio.file.Path
import scala.util.{Failure, Success, Try}

case class CommonRoutes(datadir: Path) extends ServerRoute {

  override def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("getversion", _) =>
      complete {
        val version =
          Option(EnvUtil.getVersion).map(ujson.Str).getOrElse(ujson.Null)
        val vec = Vector(("version", version))
        val obj = ujson.Obj.from(vec)
        Server.httpSuccess(obj)
      }
    case ServerCommand("zipdatadir", arr) =>
      withValidServerCommand(ZipDataDir.fromJsArr(arr)) {
        case ZipDataDir(path) =>
          complete {
            DatadirUtil.zipDatadir(datadir, path) match {
              case Success(_)  => Server.httpSuccess(ujson.Null)
              case Failure(ex) => Server.httpError(ex.getMessage)
            }
          }
      }

  }
}

case class ZipDataDir(path: Path)

object ZipDataDir {

  def fromJsArr(jsArr: ujson.Arr): Try[ZipDataDir] = {
    jsArr.arr.toList match {
      case pathJs :: Nil =>
        Try {
          val path = new File(pathJs.str).toPath
          ZipDataDir(path)
        }
      case Nil =>
        Failure(new IllegalArgumentException("Missing path argument"))

      case other =>
        Failure(
          new IllegalArgumentException(
            s"Bad number of arguments: ${other.length}. Expected: 1"))
    }
  }
}
