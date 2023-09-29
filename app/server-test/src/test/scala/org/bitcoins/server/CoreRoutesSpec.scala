package org.bitcoins.server

import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.server.routes.ServerCommand
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkitcore.dlc.DLCTestUtil
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec

class CoreRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  implicit val conf: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig()
  val coreRoutes = CoreRoutes()

  "Core routes" should {
    "decode an accept message" in {
      val args = ujson.Arr(DLCTestUtil.acceptHex)
      val route =
        coreRoutes.handleCommand(ServerCommand("decodeaccept", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        val actualJson = ujson.read(responseAs[String])
        assert(actualJson == DLCTestUtil.expectedAccept)
      }
    }

    "decode a sign message" in {
      val args = ujson.Arr(DLCTestUtil.signHex)
      val route =
        coreRoutes.handleCommand(ServerCommand("decodesign", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        val actualJson = ujson.read(responseAs[String])
        assert(actualJson == DLCTestUtil.expectedSign)
      }
    }

    "decode a gamma serialization contract info" in {
      val gammaContractInfo =
        "000000000000004e200003037965730000000000004e20026e6f0000000000002710056f74686572000000000000271000fdd824dd396910627321be0648931b2c1ce24bec294a997e717b5a606c2635b930927a5845c80e802bb8203b1eb77c3794466d68216b3548ef32b99e998640c3af6fbbe33a46bbd9eab0646b12d2fc402373b4886931e9fcb74dcf8a95e9cc2c620ef3e4fdd822790001f7952d601997eb68cc281b53ed0b34959f781fd82d947740fadb2735ca49c91a632a9a50fdd8060f000303796573026e6f056f746865723f57696c6c2074686520457468657265756d206d65726765206361757365203135206d696e75746573206f66206e6f20626c6f636b2070726f64756374696f6e"
      val args = ujson.Arr(gammaContractInfo)
      val route =
        coreRoutes.handleCommand(ServerCommand("decodecontractinfo", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        val actualJson = ujson.read(responseAs[String])
        assert(actualJson == DLCTestUtil.expectedGammaContractInfoWithV0Ann)
      }
    }
  }

}
