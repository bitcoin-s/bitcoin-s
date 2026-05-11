package org.bitcoins.cli.grpc

import org.bitcoins.core.util.EnvUtil
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.grpc.ServerGrpc
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome

import java.nio.file.Files
import scala.concurrent.{ExecutionContext, Future}

class ConsoleCliGrpcTest extends BitcoinSFixture with PostgresTestDatabase {

  override type FixtureParam = (Int, ServerGrpc)

  implicit val ec: ExecutionContext = system.dispatcher

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[(Int, ServerGrpc)] = () => {
      val tmpDir = FileUtil.tmpDir()
      val port = RpcUtil.randomPort
      val server = new ServerGrpc(tmpDir.toPath, "localhost", port)

      server.start().map(_ => (port, server))
    }

    val destroyF: ((Int, ServerGrpc)) => Future[Unit] = { case (_, server) =>
      server.stop()
    }

    makeDependentFixture[(Int, ServerGrpc)](builder, destroyF)(test)
  }

  behavior of "ConsoleCliGrpc"

  it must "execute getversion" in { case (port, _) =>
    val expected =
      ujson
        .Obj(
          "version" -> Option(EnvUtil.getVersion)
            .map(ujson.Str.apply)
            .getOrElse(ujson.Null)
        )
        .render(2)

    ConsoleCliGrpc
      .exec(Vector("--rpcport", port.toString, "getversion"))
      .map { response =>
        assert(response == expected)
      }
  }

  it must "execute zipdatadir" in { case (port, _) =>
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
          port.toString,
          "zipdatadir",
          target.toString
        ))
      .map { response =>
        assert(response.isEmpty)
        assert(Files.exists(target))
      }
  }
}
