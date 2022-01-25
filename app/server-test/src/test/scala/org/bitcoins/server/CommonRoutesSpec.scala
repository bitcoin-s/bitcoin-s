package org.bitcoins.server

import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.server.routes.{CommonRoutes, ServerCommand}
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.util.FileUtil.withTempDir
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class CommonRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  implicit val conf: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()
  val commonRoutes = CommonRoutes(conf.baseDatadir)

  "CommonRoutes" should {
    "getversion" in {

      val version =
        Option(EnvUtil.getVersion).map(v => "\"" + v + "\"").getOrElse("null")

      val expectedJson =
        ujson.read(s"""{"result":{"version":$version},"error":null}""")

      val route =
        commonRoutes.handleCommand(ServerCommand("getversion", ujson.Arr()))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        val actualJson = ujson.read(responseAs[String])
        assert(actualJson == expectedJson)
      }
    }

    "zipdatadir" in {
      withTempDir(getClass.getName) { dir =>
        val expectedJson =
          ujson.read(s"""{"result":null,"error":null}""")

        val fileName = FileUtil.randomDirName
        val dirName = FileUtil.randomDirName
        val target = dir.resolve(dirName).resolve(fileName)

        assert(!Files.exists(target))
        assert(!Files.exists(target.getParent))

        val route =
          commonRoutes.handleCommand(
            ServerCommand("zipdatadir", ujson.Arr(target.toString)))

        Post() ~> route ~> check {
          assert(contentType == ContentTypes.`application/json`)
          val actualJson = ujson.read(responseAs[String])
          assert(actualJson == expectedJson)
          assert(Files.exists(target))
        }
      }
    }
  }
}
