package org.bitcoins.server.grpc

import io.grpc.{Status, StatusRuntimeException}
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.chain.MockChainApi
import org.bitcoins.testkit.dlc.MockDLCNodeApi
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.bitcoins.testkit.node.MockNodeApi
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.wallet.MockWalletHolder
import org.scalatest.FutureOutcome

import java.nio.file.Files
import scala.concurrent.{ExecutionContext, Future}

class CommonGrpcRoutesTest extends ServerGrpcFixture {

  implicit val ec: ExecutionContext = system.dispatcher

  behavior of "CommonGrpcRoutes"

  override type GrpcClient = CommonRoutesClient
  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withCommonRoutesClient(test)
  }

  it must "getversion" in { case clientServer =>
    val client = clientServer.client
    val responseF =
      client.getVersion(GetVersionRequest())
    val expectedVersion = Option(EnvUtil.getVersion)
    responseF.map { response =>
      assert(response.version == expectedVersion)
    }
  }

  it must "zipdatadir" in { case clientServer =>
    val client = clientServer.client
    val fileName = FileUtil.randomDirName
    val dirName = FileUtil.randomDirName
    val dir = FileUtil.tmpDir().toPath
    val target = dir.resolve(dirName).resolve(fileName)

    assert(!Files.exists(target))
    assert(!Files.exists(target.getParent))

    client.zipDataDir(ZipDataDirRequest(path = target.toString)).map { _ =>
      assert(Files.exists(target))
    }
  }

  it must "authenticate with the configured password" in { _ =>
    val password = "topsecret"
    val port = RpcUtil.randomPort
    val dlcNode = MockDLCNodeApi.fresh()
    val walletApi = MockWalletHolder.emptyApi()
    val server = new ServerGrpc(
      datadir = FileUtil.tmpDir().toPath,
      rpchost = "localhost",
      port = port,
      rpcPassword = password,
      chainApi = MockChainApi,
      network = network,
      startedTorConfigF = Future.unit,
      nodeApiF = Future.successful(MockNodeApi),
      dlcNodeF = Future.successful(dlcNode),
      walletApiF = Future.successful(walletApi)
    )
    val clientSettings = org.apache.pekko.grpc.GrpcClientSettings
      .connectToServiceAt("localhost", port)
      .withTls(false)
      .withCallCredentials(GrpcAuth.basicCallCredentials(password))
    val client = CommonRoutesClient(clientSettings)

    val resultF = for {
      _ <- server.start()
      response <- client.getVersion(GetVersionRequest())
    } yield response

    resultF
      .transformWith { result =>
        client
          .close()
          .flatMap(_ => server.stop())
          .transform(_ => result)
      }
      .map { response =>
        assert(response.version == Option(EnvUtil.getVersion))
      }
  }

  it must "reject authentication with an invalid password" in { _ =>
    val serverPassword = "topsecret"
    val clientPassword = "wrong-password"
    val port = RpcUtil.randomPort
    val dlcNode = MockDLCNodeApi.fresh()
    val walletApi = MockWalletHolder.emptyApi()
    val server = new ServerGrpc(
      datadir = FileUtil.tmpDir().toPath,
      rpchost = "localhost",
      port = port,
      rpcPassword = serverPassword,
      chainApi = MockChainApi,
      network = network,
      startedTorConfigF = Future.unit,
      nodeApiF = Future.successful(MockNodeApi),
      dlcNodeF = Future.successful(dlcNode),
      walletApiF = Future.successful(walletApi)
    )
    val clientSettings = org.apache.pekko.grpc.GrpcClientSettings
      .connectToServiceAt("localhost", port)
      .withTls(false)
      .withCallCredentials(GrpcAuth.basicCallCredentials(clientPassword))
    val client = CommonRoutesClient(clientSettings)

    val resultF = for {
      _ <- server.start()
      err <- client.getVersion(GetVersionRequest()).failed
    } yield err

    resultF
      .transformWith { result =>
        client
          .close()
          .flatMap(_ => server.stop())
          .transform(_ => result)
      }
      .map { err =>
        assert(err.isInstanceOf[StatusRuntimeException])
        val grpcErr = err.asInstanceOf[StatusRuntimeException]
        assert(grpcErr.getStatus.getCode == Status.Code.UNAUTHENTICATED)
      }
  }
}
