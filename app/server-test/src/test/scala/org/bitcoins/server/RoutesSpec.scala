package org.bitcoins.server

import java.time.{ZoneId, ZonedDateTime}

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.server.ValidationRejection
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.{
  BlockHash,
  BlockHeight,
  BlockTime,
  InvalidBlockStamp
}
import org.bitcoins.core.protocol.transaction.EmptyTransaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.node.Node
import org.bitcoins.wallet.MockUnlockedWalletApi
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import ujson.Value.InvalidData
import ujson.{Arr, Null, Num, Str}

import scala.concurrent.Future

class RoutesSpec
    extends WordSpec
    with Matchers
    with ScalatestRouteTest
    with MockFactory {

  // the genesis address
  val testAddressStr = "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
  val testAddress = BitcoinAddress(testAddressStr).get

  val mockWalletApi = mock[MockUnlockedWalletApi]

  val mockChainApi = mock[ChainApi]

  val mockNode = mock[Node]

  val chainRoutes = ChainRoutes(mockChainApi)

  val nodeRoutes = NodeRoutes(mockNode)

  val walletRoutes = WalletRoutes(mockWalletApi, mockNode)

  "The server" should {

    "return the block count" in {
      (mockChainApi.getBlockCount: () => Future[Int])
        .expects()
        .returning(Future.successful(1234567890))

      val route =
        chainRoutes.handleCommand(ServerCommand("getblockcount", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":1234567890,"error":null}"""
      }
    }

    "return the best block hash" in {
      (mockChainApi.getBestBlockHash: () => Future[DoubleSha256DigestBE])
        .expects()
        .returning(Future.successful(DoubleSha256DigestBE.empty))

      val route =
        chainRoutes.handleCommand(ServerCommand("getbestblockhash", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }
    }

    "return the wallet's balance" in {
      (mockWalletApi.getBalance _)
        .expects()
        .returning(Future.successful(Bitcoins(50)))

      val route =
        walletRoutes.handleCommand(ServerCommand("getbalance", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":50,"error":null}"""
      }
    }

    "return a new address" in {
      (mockWalletApi.getNewAddress: () => Future[BitcoinAddress])
        .expects()
        .returning(Future.successful(testAddress))

      val route =
        walletRoutes.handleCommand(ServerCommand("getnewaddress", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"""" + testAddressStr + """","error":null}"""
      }
    }

    "send to an address" in {
      // positive cases

      (mockWalletApi
        .sendToAddress(_: BitcoinAddress, _: CurrencyUnit, _: FeeUnit))
        .expects(testAddress, Bitcoins(100), *)
        .returning(Future.successful(EmptyTransaction))

      (mockNode.broadcastTransaction _)
        .expects(EmptyTransaction)
        .returning(FutureUtil.unit)
        .anyNumberOfTimes()

      val route = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Str(testAddressStr), Num(100))))

      Post() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }

      // negative cases

      val route1 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Null, Null)))

      Post() ~> route1 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Str")))
      }

      val route2 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr("Null", Null)))

      Post() ~> route2 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData("Null", "Expected a valid address")))
      }

      val route3 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Str(testAddressStr), Null)))

      Post() ~> route3 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Num")))
      }

      val route4 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Str(testAddressStr), Str("abc"))))

      Post() ~> route4 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData("abc", "Expected ujson.Num")))
      }

    }

    "return the peer list" in {
      val route =
        nodeRoutes.handleCommand(ServerCommand("getpeers", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"TODO implement getpeers","error":null}"""
      }
    }

    "run wallet rescan" in {
      // positive cases

      (mockWalletApi.rescan _)
        .expects(Vector(testAddress.scriptPubKey), None, None)
        .returning(FutureUtil.unit)

      val route1 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr(Str(testAddressStr)), Null, Null)))

      Post() ~> route1 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"ok","error":null}"""
      }

      (mockWalletApi.rescan _)
        .expects(
          Vector(testAddress.scriptPubKey),
          Some(BlockTime(
            ZonedDateTime.of(2018, 10, 27, 12, 34, 56, 0, ZoneId.of("UTC")))),
          None)
        .returning(FutureUtil.unit)

      val route2 =
        walletRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Arr(Str(testAddressStr)), Str("2018-10-27T12:34:56Z"), Null)))

      Post() ~> route2 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"ok","error":null}"""
      }

      (mockWalletApi.rescan _)
        .expects(Vector(testAddress.scriptPubKey),
                 None,
                 Some(BlockHash(DoubleSha256DigestBE.empty)))
        .returning(FutureUtil.unit)

      val route3 =
        walletRoutes.handleCommand(
          ServerCommand("rescan",
                        Arr(Arr(Str(testAddressStr)),
                            Null,
                            Str(DoubleSha256DigestBE.empty.hex))))

      Post() ~> route3 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"ok","error":null}"""
      }

      (mockWalletApi.rescan _)
        .expects(Vector(testAddress.scriptPubKey),
                 Some(BlockHeight(12345)),
                 Some(BlockHeight(67890)))
        .returning(FutureUtil.unit)

      val route4 =
        walletRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Arr(Str(testAddressStr)), Str("12345"), Num(67890))))

      Post() ~> route4 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"ok","error":null}"""
      }

      // negative cases

      val route5 =
        walletRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Arr(Str(testAddressStr)), Str("abcd"), Str("efgh"))))

      Post() ~> route5 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidBlockStamp("abcd")))
      }

      val route6 =
        walletRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Arr(Str(testAddressStr)), Null, Str("2018-10-27T12:34:56"))))

      Post() ~> route6 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidBlockStamp("2018-10-27T12:34:56")))
      }

      val route7 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr(Str(testAddressStr)), Num(-1), Null)))

      Post() ~> route7 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData(Num(-1), "Expected a positive integer")))
      }

      val route8 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr(), Null, Null)))

      Post() ~> route8 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData(Arr(), "Expected a non-empty address array")))
      }

      val route9 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr("abcdefgh"), Null, Null)))

      Post() ~> route9 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData("abcdefgh", "Expected a valid address")))
      }

    }

  }
}
