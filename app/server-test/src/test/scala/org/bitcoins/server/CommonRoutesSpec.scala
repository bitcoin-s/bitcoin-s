package org.bitcoins.server

import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.server.routes.{CommonRoutes, ServerCommand}
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec

class CommonRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  val commonRoutes = CommonRoutes()

  "CommonRoutes" should {
    "getversion" in {

      val route =
        commonRoutes.handleCommand(ServerCommand("getversion", ujson.Arr()))
      Get() ~> route ~> check {
        s"""
           |{ "version" : "${getClass.getPackage.getImplementationVersion}" }
           |""".stripMargin
      }
    }
  }
}
