package org.bitcoins.server

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.crypto.DoubleSha256DigestBE
import org.scalatest.{Matchers, WordSpec}
import ujson.{Arr, Null, Num, Str}

class RoutesSpec extends WordSpec with Matchers with ScalatestRouteTest {

  import MockObjects._

  val mockChainRoutes = ChainRoutes(mockChainApi)

  val mockNodeRoutes = NodeRoutes(mockNode)

  val mockWalletRoutes = WalletRoutes(mockWalletApi, mockNode)

  "The server" should {

    "return the block count" in {
      val route =
        mockChainRoutes.handleCommand(ServerCommand("getblockcount", Arr()))
      Get() ~> route ~> check {
        responseAs[String] shouldEqual """{"result":1234567890,"error":null}"""
      }
    }

    "return the best block hash" in {
      val route =
        mockChainRoutes.handleCommand(ServerCommand("getbestblockhash", Arr()))
      Get() ~> route ~> check {
        responseAs[String] shouldEqual """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }
    }

    "return the wallet's balance" in {
      val route =
        mockWalletRoutes.handleCommand(ServerCommand("getbalance", Arr()))
      Get() ~> route ~> check {
        responseAs[String] shouldEqual """{"result":50,"error":null}"""
      }
    }

    "return the peer list" in {
      val route =
        mockNodeRoutes.handleCommand(ServerCommand("getpeers", Arr()))
      Get() ~> route ~> check {
        responseAs[String] shouldEqual """{"result":"TODO implement getpeers","error":null}"""
      }
    }

    "run wallet rescan" in {
      val route1 =
        mockNodeRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr(Str(testAddress)), Null, Null)))
      Get() ~> route1 ~> check {
        responseAs[String] shouldEqual """{"result":"ok","error":null}"""
      }

      val route2 =
        mockNodeRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Arr(Str(testAddress)), Str("2018-10-27T12:34:56Z"), Null)))
      Get() ~> route2 ~> check {
        responseAs[String] shouldEqual """{"result":"ok","error":null}"""
      }

      val route3 =
        mockNodeRoutes.handleCommand(
          ServerCommand("rescan",
                        Arr(Arr(Str(testAddress)),
                            Null,
                            Str(DoubleSha256DigestBE.empty.hex))))
      Get() ~> route3 ~> check {
        responseAs[String] shouldEqual """{"result":"ok","error":null}"""
      }

      val route4 =
        mockNodeRoutes.handleCommand(
          ServerCommand("rescan",
                        Arr(Arr(Str(testAddress)), Num(12345), Num(67890))))
      Get() ~> route1 ~> check {
        responseAs[String] shouldEqual """{"result":"ok","error":null}"""
      }
    }

    "return a new address" in {
      val route =
        mockWalletRoutes.handleCommand(ServerCommand("getnewaddress", Arr()))
      Get() ~> route ~> check {
        responseAs[String] shouldEqual """{"result":"""" + testAddress + """","error":null}"""
      }
    }

    "send to an address" in {
      val route = mockWalletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Str(testAddress), Num(100))))
      Get() ~> route ~> check {
        responseAs[String] shouldEqual """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }
    }
  }
}
