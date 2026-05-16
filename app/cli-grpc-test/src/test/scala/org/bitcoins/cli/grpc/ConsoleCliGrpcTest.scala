package org.bitcoins.cli.grpc

import io.grpc.{Status, StatusRuntimeException}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.grpc.ServerGrpc
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.testkit.chain.StaticChainApi
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.scalatest.FutureOutcome

import java.nio.file.Files
import scala.concurrent.{ExecutionContext, Future}

class ConsoleCliGrpcTest extends BitcoinSFixture with PostgresTestDatabase {
  private val rpcPassword = "topsecret"

  override type FixtureParam = (Int, ServerGrpc)

  implicit val ec: ExecutionContext = system.dispatcher

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[(Int, ServerGrpc)] = () => {
      val tmpDir = FileUtil.tmpDir()
      val port = RpcUtil.randomPort
      val headerDb = ChainTestUtil.regTestGenesisHeaderDb
      val chainApi = StaticChainApi(
        bestHeader = headerDb,
        blockCount = 1,
        filterCount = 2,
        filterHeaderCount = 3,
        syncing = false,
        isIBDValue = false,
        medianTimePast = 123L
      )
      val server =
        new ServerGrpc(tmpDir.toPath,
                       "localhost",
                       port,
                       rpcPassword = rpcPassword,
                       chainApiOpt = Some(chainApi),
                       networkOpt = Some(RegTest),
                       startedTorConfigF = Future.unit)

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
      .exec(
        Vector(
          "--rpcport",
          port.toString,
          "--password",
          rpcPassword,
          "getversion"
        ))
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
          "--password",
          rpcPassword,
          "zipdatadir",
          target.toString
        ))
      .map { response =>
        assert(response.isEmpty)
        assert(Files.exists(target))
      }
  }

  it must "execute getblockcount" in { case (port, _) =>
    ConsoleCliGrpc
      .exec(
        Vector(
          "--rpcport",
          port.toString,
          "--password",
          rpcPassword,
          "getblockcount"
        ))
      .map { response =>
        assert(response == "1")
      }
  }

  it must "execute getbestblockhash" in { case (port, _) =>
    val expected = ChainTestUtil.regTestGenesisHeaderDb.hashBE.hex
    ConsoleCliGrpc
      .exec(
        Vector(
          "--rpcport",
          port.toString,
          "--password",
          rpcPassword,
          "getbestblockhash"
        ))
      .map { response =>
        assert(response == expected)
      }
  }

  it must "fail authentication when an invalid password is provided" in {
    case (port, _) =>
      ConsoleCliGrpc
        .exec(
          Vector(
            "--rpcport",
            port.toString,
            "--password",
            "bad-password",
            "getversion"
          ))
        .failed
        .map { err =>
          assert(err.isInstanceOf[StatusRuntimeException])
          val grpcErr = err.asInstanceOf[StatusRuntimeException]
          assert(grpcErr.getStatus.getCode == Status.Code.UNAUTHENTICATED)
        }
  }
}
