package org.bitcoins.server.grpc

import org.bitcoins.core.config.RegTest
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.PostgresTestDatabase
import org.bitcoins.testkit.chain.MockChainApi
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.node.MockNodeApi
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkitcore.chain.ChainTestUtil
import org.scalatest.FutureOutcome

import scala.concurrent.{ExecutionContext, Future}

class ChainGrpcRoutesTest extends BitcoinSFixture with PostgresTestDatabase {
  private val network = RegTest
  override type FixtureParam = (ChainRoutesClient, ServerGrpc)

  implicit val ec: ExecutionContext = system.dispatcher

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val builder: () => Future[(ChainRoutesClient, ServerGrpc)] = () => {
      val tmpDir = FileUtil.tmpDir()
      val port = RpcUtil.randomPort
      val host = "localhost"

      val server = new ServerGrpc(tmpDir.toPath,
                                  host,
                                  port,
                                  rpcPassword = "",
                                  chainApi = MockChainApi,
                                  network = network,
                                  startedTorConfigF = Future.unit,
                                  nodeApi = MockNodeApi)
      val clientSettings = org.apache.pekko.grpc.GrpcClientSettings
        .connectToServiceAt(host, port)
        .withTls(false)
      val client = ChainRoutesClient(clientSettings)

      server.start().map(_ => (client, server))
    }

    val destroyF: ((ChainRoutesClient, ServerGrpc)) => Future[Unit] = {
      case (client, server) =>
        for {
          _ <- client.close()
          _ <- server.stop()
        } yield ()
    }

    makeDependentFixture[(ChainRoutesClient, ServerGrpc)](builder, destroyF)(
      test)
  }

  behavior of "ChainGrpcRoutes"

  it must "getinfo" in { case (client, _) =>
    client.getInfo(GetInfoRequest()).map { response =>
      assert(response.network == RegTest.name)
      assert(
        response.blockHeight == ChainTestUtil.regTestGenesisHeaderDb.height)
      assert(
        response.blockHash == ChainTestUtil.regTestGenesisHeaderDb.hashBE.hex)
      assert(response.torStarted)
      assert(!response.syncing)
      assert(!response.isInitialBlockDownload)
    }
  }

  it must "getblockcount" in { case (client, _) =>
    client.getBlockCount(GetBlockCountRequest()).map { response =>
      assert(response.count == 0)
    }
  }

  it must "getfiltercount" in { case (client, _) =>
    client.getFilterCount(GetFilterCountRequest()).map { response =>
      assert(response.count == 0)
    }
  }

  it must "getfilterheadercount" in { case (client, _) =>
    client.getFilterHeaderCount(GetFilterHeaderCountRequest()).map { response =>
      assert(response.count == 0)
    }
  }

  it must "getbestblockhash" in { case (client, _) =>
    for {
      response <- client.getBestBlockHash(GetBestBlockHashRequest())
      expectedHash <- MockChainApi.getBestBlockHash()
    } yield {
      assert(response.hash == expectedHash.hex)
    }
  }

  it must "getblockheader" in { case (client, _) =>
    val hash = ChainTestUtil.regTestGenesisHeaderDb.hashBE.hex
    client.getBlockHeader(GetBlockHeaderRequest(hash = hash)).map { response =>
      assert(response.header.isEmpty)
    }
  }

  it must "getmediantimepast" in { case (client, _) =>
    client.getMedianTimePast(GetMedianTimePastRequest()).map { response =>
      assert(response.mediantimepast == 0L)
    }
  }
}
