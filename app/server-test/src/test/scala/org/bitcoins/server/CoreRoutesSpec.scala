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
    BitcoinSTestAppConfig.getSpvTestConfig()
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
  }

}
