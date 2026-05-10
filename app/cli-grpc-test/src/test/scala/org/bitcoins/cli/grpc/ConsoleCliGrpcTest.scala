package org.bitcoins.cli.grpc

import org.bitcoins.core.util.EnvUtil
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.bitcoins.testkit.util.FileUtil

import java.nio.file.Files
import scala.concurrent.ExecutionContext

class ConsoleCliGrpcTest extends ServerGrpcFixture {

  implicit val ec: ExecutionContext = system.dispatcher

  behavior of "ConsoleCliGrpc"

  it must "execute getversion" in { case (_, server) =>
    val expected =
      ujson
        .Obj(
          "version" -> Option(EnvUtil.getVersion)
            .map(ujson.Str.apply)
            .getOrElse(ujson.Null)
        )
        .render(2)

    ConsoleCliGrpc
      .exec(Vector("--rpcport", server.port.toString, "getversion"))
      .map { response =>
        assert(response == expected)
      }
  }

  it must "execute zipdatadir" in { case (_, server) =>
    val fileName = FileUtil.randomDirName
    val dirName = FileUtil.randomDirName
    val dir = FileUtil.tmpDir().toPath
    val target = dir.resolve(dirName).resolve(fileName)

    assert(!Files.exists(target))
    assert(!Files.exists(target.getParent))

    ConsoleCliGrpc
      .exec(
        Vector(
          "--rpcport",
          server.port.toString,
          "zipdatadir",
          target.toString
        ))
      .map { response =>
        assert(response.isEmpty)
        assert(Files.exists(target))
      }
  }
}
