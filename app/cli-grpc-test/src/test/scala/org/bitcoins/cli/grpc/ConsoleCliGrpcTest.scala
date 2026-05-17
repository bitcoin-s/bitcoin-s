package org.bitcoins.cli.grpc

import io.grpc.{Status, StatusRuntimeException}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.commons.jsonmodels.BitcoinSServerInfo
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.server.grpc.ServerGrpc
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.testkit.chain.MockChainApi
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.scalatest.FutureOutcome

import java.nio.file.Files
import scala.concurrent.{ExecutionContext, Future}

class ConsoleCliGrpcTest extends BitcoinSFixture with PostgresTestDatabase {
  private val rpcPassword = "topsecret"
  private val network = RegTest
  override type FixtureParam = (Int, ServerGrpc)

  implicit val ec: ExecutionContext = system.dispatcher

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[(Int, ServerGrpc)] = () => {
      val tmpDir = FileUtil.tmpDir()
      val port = RpcUtil.randomPort
      val server =
        new ServerGrpc(tmpDir.toPath,
                       "localhost",
                       port,
                       rpcPassword = rpcPassword,
                       chainApi = MockChainApi,
                       network = network,
                       startedTorConfigF = Future.unit)

      server.start().map(_ => (port, server))
    }

    val destroyF: ((Int, ServerGrpc)) => Future[Unit] = { case (_, server) =>
      server.stop()
    }

    makeDependentFixture[(Int, ServerGrpc)](builder, destroyF)(test)
  }

  behavior of "ConsoleCliGrpc"

  private def exec(port: Int, args: String*): Future[String] = {
    ConsoleCliGrpc.exec(
      Vector("--rpcport", port.toString, "--password", rpcPassword) ++ args
    )
  }

  it must "execute getversion" in { case (port, _) =>
    val expected =
      ujson
        .Obj(
          "version" -> Option(EnvUtil.getVersion)
            .map(ujson.Str.apply)
            .getOrElse(ujson.Null)
        )
        .render(2)

    exec(port, "getversion").map { response =>
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

    exec(port, "zipdatadir", target.toString).map { response =>
      assert(response.isEmpty)
      assert(Files.exists(target))
    }
  }

  it must "execute getinfo" in { case (port, _) =>
    val expected = BitcoinSServerInfo(
      network = network,
      blockHeight = ChainTestUtil.regTestGenesisHeaderDb.height,
      blockHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE,
      torStarted = true,
      syncing = false,
      isInitialBlockDownload = false
    ).toJson.render(2)

    exec(port, "getinfo").map { response =>
      assert(response == expected)
    }
  }

  it must "execute getblockcount" in { case (port, _) =>
    exec(port, "getblockcount").map { response =>
      assert(response == "0")
    }
  }

  it must "execute getfiltercount" in { case (port, _) =>
    exec(port, "getfiltercount").map { response =>
      assert(response == "0")
    }
  }

  it must "execute getfilterheadercount" in { case (port, _) =>
    exec(port, "getfilterheadercount").map { response =>
      assert(response == "0")
    }
  }

  it must "execute getbestblockhash" in { case (port, _) =>
    exec(port, "getbestblockhash").map { response =>
      assert(response == DoubleSha256DigestBE.empty.hex)
    }
  }

  it must "execute getblockheader" in { case (port, _) =>
    val blockHash = ChainTestUtil.regTestGenesisHeaderDb.hashBE.hex

    exec(port, "getblockheader", blockHash).map { response =>
      assert(response == "null")
    }
  }

  it must "execute getmediantimepast" in { case (port, _) =>
    exec(port, "getmediantimepast").map { response =>
      assert(response == "0")
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
