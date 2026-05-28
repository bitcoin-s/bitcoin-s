package org.bitcoins.cli.grpc

import io.grpc.{Status, StatusRuntimeException}
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.commons.jsonmodels.BitcoinSServerInfo
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPublicKey}
import org.bitcoins.server.grpc.CommonRoutesClient
import org.bitcoins.testkit.dlc.MockDLCNodeApi
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome

import java.nio.file.Files
import scala.concurrent.{ExecutionContext, Future}

class ConsoleCliGrpcTest extends ServerGrpcFixture {
  private val rpcPassword = "topsecret"

  implicit val ec: ExecutionContext = system.dispatcher
  override type GrpcClient = CommonRoutesClient
  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withCommonRoutesClient(test, rpcPassword = rpcPassword)
  }

  behavior of "ConsoleCliGrpc"

  private def exec(port: Int, args: String*): Future[String] = {
    ConsoleCliGrpc.exec(
      Vector("--rpcport", port.toString, "--password", rpcPassword) ++ args
    )
  }

  it must "execute getversion" in { case clientServer =>
    val port = clientServer.server.port
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

  it must "execute zipdatadir" in { case clientServer =>
    val port = clientServer.server.port
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

  it must "execute getinfo" in { case clientServer =>
    val port = clientServer.server.port
    val chainApi = clientServer.chainApi
    for {
      blockCount <- chainApi.getBlockCount()
      bestBlockHash <- chainApi.getBestBlockHash()
      response <- exec(port, "getinfo")
    } yield {
      val expected = BitcoinSServerInfo(
        network = network,
        blockHeight = blockCount,
        blockHash = bestBlockHash,
        torStarted = true,
        syncing = false,
        isInitialBlockDownload = false
      ).toJson.render(2)
      assert(response == expected)
    }
  }

  it must "execute getblockcount" in { case clientServer =>
    val port = clientServer.server.port
    val chainApi = clientServer.chainApi
    for {
      blockCount <- chainApi.getBlockCount()
      response <- exec(port, "getblockcount")
    } yield {
      assert(response == blockCount.toString)
    }
  }

  it must "execute getfiltercount" in { case clientServer =>
    val port = clientServer.server.port
    val chainApi = clientServer.chainApi
    for {
      filterCount <- chainApi.getFilterCount()
      response <- exec(port, "getfiltercount")
    } yield {
      assert(response == filterCount.toString)
    }
  }

  it must "execute getfilterheadercount" in { case clientServer =>
    val port = clientServer.server.port
    val chainApi = clientServer.chainApi
    for {
      filterHeaderCount <- chainApi.getFilterHeaderCount()
      response <- exec(port, "getfilterheadercount")
    } yield {
      assert(response == filterHeaderCount.toString)
    }
  }

  it must "execute getbestblockhash" in { case clientServer =>
    val port = clientServer.server.port
    val chainApi = clientServer.chainApi
    for {
      bestBlockHash <- chainApi.getBestBlockHash()
      response <- exec(port, "getbestblockhash")
    } yield {
      assert(response == bestBlockHash.hex)
    }
  }

  it must "execute getblockheader" in { case clientServer =>
    val port = clientServer.server.port
    // Use an unknown block hash - should return null
    val unknownHash = DoubleSha256DigestBE.empty.hex

    exec(port, "getblockheader", unknownHash).map { response =>
      assert(response == "null")
    }
  }

  it must "execute getmediantimepast" in { case clientServer =>
    val port = clientServer.server.port
    val chainApi = clientServer.chainApi
    for {
      medianTimePast <- chainApi.getMedianTimePast()
      response <- exec(port, "getmediantimepast")
    } yield {
      assert(response == medianTimePast.toString)
    }
  }

  it must "execute getconnectioncount" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getconnectioncount").map { response =>
      assert(response.toInt >= 0)
    }
  }

  it must "execute isempty" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "isempty").map { response =>
      assert(response == "true")
    }
  }

  it must "execute getbalance" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getbalance").map { response =>
      assert(response == "0")
    }
  }

  it must "execute getconfirmedbalance" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getconfirmedbalance").map { response =>
      assert(response == "0")
    }
  }

  it must "execute getunconfirmedbalance" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getunconfirmedbalance").map { response =>
      assert(response == "0")
    }
  }

  it must "execute getbalances" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getbalances").map { response =>
      val json = ujson.read(response)
      assert(json("confirmed").num == 0)
      assert(json("unconfirmed").num == 0)
      assert(json("reserved").num == 0)
      assert(json("total").num == 0)
    }
  }

  it must "execute getutxos" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getutxos").map { response =>
      assert(response == "[]")
    }
  }

  it must "execute getreservedutxos" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getreservedutxos").map { response =>
      assert(response == "[]")
    }
  }

  it must "execute getaddresses" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getaddresses").map { response =>
      assert(response == "[]")
    }
  }

  it must "execute getspentaddresses" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getspentaddresses").map { response =>
      assert(response == "[]")
    }
  }

  it must "execute getfundedaddresses" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getfundedaddresses").map { response =>
      assert(response == "[]")
    }
  }

  it must "execute getunusedaddresses" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getunusedaddresses").map { response =>
      assert(response == "[]")
    }
  }

  it must "execute getaccounts" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getaccounts").map { response =>
      val accounts = ujson.read(response).arr
      assert(accounts.nonEmpty)
    }
  }

  it must "execute getaddresslabels" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getaddresslabels").map { response =>
      assert(response == "[]")
    }
  }

  it must "execute getdlchostaddress" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getdlchostaddress").map { response =>
      val expected =
        s"${MockDLCNodeApi.hostAddress.getHostName}:${MockDLCNodeApi.hostAddress.getPort}"
      assert(response == expected)
    }
  }

  it must "execute offers-list" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "incoming-offers-list").map { response =>
      assert(response == "[]")
    }
  }

  it must "execute addoffer" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "addoffer", MockDLCNodeApi.offerLnMessageHex, "note", "peer-1")
      .map { response =>
        assert(response.nonEmpty)
      }
  }

  it must "execute removeoffer" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "removeoffer", org.bitcoins.crypto.Sha256Digest.empty.hex).map {
      response =>
        assert(response == org.bitcoins.crypto.Sha256Digest.empty.hex)
    }
  }

  it must "execute contact-add and contacts-list" in { case clientServer =>
    val port = clientServer.server.port
    for {
      addResponse <- exec(port,
                          "contact-add",
                          "alice",
                          "localhost:2862",
                          "memo")
      listResponse <- exec(port, "contacts-list")
    } yield {
      assert(addResponse == "ok")
      val contacts = ujson.read(listResponse).arr
      assert(contacts.nonEmpty)
      assert(contacts.head.obj("alias").str == "alice")
    }
  }

  it must "execute dlc-contact-add" in { case clientServer =>
    val port = clientServer.server.port
    exec(port,
         "dlc-contact-add",
         org.bitcoins.crypto.Sha256Digest.empty.hex,
         "localhost:2862").map { response =>
      val json = ujson.read(response)
      assert(
        json.obj("dlcId").str == org.bitcoins.crypto.Sha256Digest.empty.hex)
      assert(json.obj("contactId").str == "localhost:2862")
    }
  }

  it must "execute offer-send" in { case clientServer =>
    val port = clientServer.server.port
    exec(port,
         "offer-send",
         org.bitcoins.crypto.Sha256Digest.empty.hex,
         "localhost:2862",
         "message").map { response =>
      assert(response == org.bitcoins.crypto.Sha256Digest.empty.hex)
    }
  }

  it must "execute walletinfo" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "walletinfo").map { response =>
      val json = ujson.read(response)
      assert(json.obj.contains("walletName"))
      assert(json.obj.contains("rootXpub"))
      assert(json.obj.contains("xpub"))
      assert(json.obj.contains("hdAccount"))
      assert(json.obj.contains("height"))
      assert(json.obj.contains("blockHash"))
    }
  }

  it must "execute getnewaddress" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getnewaddress").map { response =>
      assert(response.nonEmpty)
    }
  }

  it must "execute getnewaddress with label" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "getnewaddress", "--label", "test-label").map { response =>
      assert(response.nonEmpty)
    }
  }

  it must "execute gettransaction for unknown txid" in { case clientServer =>
    val port = clientServer.server.port
    val zeroTxId = "0" * 64
    exec(port, "gettransaction", zeroTxId).map { response =>
      assert(response == "null")
    }
  }

  it must "execute getaddresstags returns empty for fresh address" in {
    case clientServer =>
      val port = clientServer.server.port
      for {
        addrResponse <- exec(port, "getnewaddress")
        tagsResponse <- exec(port, "getaddresstags", addrResponse)
      } yield {
        assert(tagsResponse == "[]")
      }
  }

  it must "execute getaddresslabel returns empty for fresh address" in {
    case clientServer =>
      val port = clientServer.server.port
      for {
        addrResponse <- exec(port, "getnewaddress")
        labelResponse <- exec(port, "getaddresslabel", addrResponse)
      } yield {
        assert(labelResponse == "[]")
      }
  }

  it must "execute labeladdress then getaddresstags" in { case clientServer =>
    val port = clientServer.server.port
    val label = "my-test-label"
    for {
      addrResponse <- exec(port, "getnewaddress")
      labelResult <- exec(port, "labeladdress", addrResponse, label)
      tagsResponse <- exec(port, "getaddresstags", addrResponse)
    } yield {
      assert(labelResult.contains(label))
      val tags = ujson.read(tagsResponse).arr.map(_.str)
      assert(tags.contains(label))
    }
  }

  it must "execute labeladdress then getaddresslabel" in { case clientServer =>
    val port = clientServer.server.port
    val label = "label-for-label-test"
    for {
      addrResponse <- exec(port, "getnewaddress")
      _ <- exec(port, "labeladdress", addrResponse, label)
      labelResponse <- exec(port, "getaddresslabel", addrResponse)
    } yield {
      val labels = ujson.read(labelResponse).arr.map(_.str)
      assert(labels.contains(label))
    }
  }

  it must "execute dropaddresslabel" in { case clientServer =>
    val port = clientServer.server.port
    val label = "drop-me"
    for {
      addrResponse <- exec(port, "getnewaddress")
      _ <- exec(port, "labeladdress", addrResponse, label)
      dropResult <- exec(port, "dropaddresslabel", addrResponse, label)
      tagsAfter <- exec(port, "getaddresstags", addrResponse)
    } yield {
      assert(dropResult.contains("dropped"))
      val tags = ujson.read(tagsAfter).arr.map(_.str)
      assert(!tags.contains(label))
    }
  }

  it must "execute dropaddresslabels" in { case clientServer =>
    val port = clientServer.server.port
    for {
      addrResponse <- exec(port, "getnewaddress")
      _ <- exec(port, "labeladdress", addrResponse, "lbl1")
      _ <- exec(port, "labeladdress", addrResponse, "lbl2")
      dropResult <- exec(port, "dropaddresslabels", addrResponse)
      tagsAfter <- exec(port, "getaddresstags", addrResponse)
    } yield {
      assert(dropResult.nonEmpty)
      assert(tagsAfter == "[]")
    }
  }

  it must "execute lockunspent unlock with empty wallet" in {
    case clientServer =>
      val port = clientServer.server.port
      exec(port, "lockunspent", "true", "[]").map { response =>
        assert(response == "false")
      }
  }

  it must "execute getaddressinfo for a known address" in { case clientServer =>
    val port = clientServer.server.port
    for {
      addrResponse <- exec(port, "getnewaddress")
      infoResponse <- exec(port, "getaddressinfo", addrResponse)
    } yield {
      val json = ujson.read(infoResponse)
      assert(!json("pubkey").isNull)
      assert(json("pubkey").str.nonEmpty)
    }
  }

  it must "execute getaddressinfo for unknown address returns null pubkey" in {
    case clientServer =>
      val port = clientServer.server.port
      val p2wpkh = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)
      val unknownAddr = BitcoinAddress.fromScriptPubKey(p2wpkh, RegTest)
      exec(port, "getaddressinfo", unknownAddr.toString).map { response =>
        val json = ujson.read(response)
        assert(json("pubkey").isNull)
        assert(json("hdPath").isNull)
      }
  }

  it must "execute createnewaccount with segwit purpose" in {
    case clientServer =>
      val port = clientServer.server.port
      exec(port, "createnewaccount", HDPurpose.SegWit.toString).map {
        response =>
          // response is a JSON array of xpubs
          val xpubs = ujson.read(response).arr
          assert(xpubs.nonEmpty)
      }
  }

  it must "execute rescan" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "rescan", "--ignorecreationtime").map { response =>
      assert(
        response.contains("Rescan started.") || response.contains(
          "Rescan done."))
    }
  }

  it must "execute estimatefee" in { case clientServer =>
    val port = clientServer.server.port
    exec(port, "estimatefee").map { response =>
      val value = response.toDouble
      assert(value >= -1.0)
    }
  }

  it must "fail authentication when an invalid password is provided" in {
    case clientServer =>
      val port = clientServer.server.port
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
