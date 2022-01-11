package org.bitcoins.server

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.server.routes.ServerCommand
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.wallet.MockWalletApi
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import akka.http.scaladsl.model.ContentTypes._
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}

import scala.concurrent.Future

class WalletRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  implicit val conf: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()
  val mockWalletApi = mock[MockWalletApi]

  val walletRoutes: WalletRoutes =
    WalletRoutes(mockWalletApi)(system, conf.walletConf)
  "WalletRoutes" should {
    "estimatefee" in {

      (mockWalletApi.getFeeRate: () => Future[FeeUnit])
        .expects()
        .returning(Future.successful(SatoshisPerVirtualByte.one))
      val route =
        walletRoutes.handleCommand(ServerCommand("estimatefee", ujson.Arr()))

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[String] == s"""{"result":1,"error":null}""")
      }
    }
  }

}
