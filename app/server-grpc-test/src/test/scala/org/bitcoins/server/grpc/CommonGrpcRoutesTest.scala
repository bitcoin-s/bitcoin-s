package org.bitcoins.server.grpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.apache.pekko.http.scaladsl.Http
import org.bitcoins.core.util.EnvUtil
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.util.FileUtil.withTempDir
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}

class CommonGrpcRoutesTest extends AnyWordSpec with BeforeAndAfterAll {

  implicit val system: ActorSystem = ActorSystem("CommonGrpcRoutesTest")
  implicit val ec: ExecutionContext = system.dispatcher

  private var serverBinding: Http.ServerBinding = _
  private var client: CommonRoutesClient = _
  private val testDatadir = Files.createTempDirectory("server-grpc-test")

  override def beforeAll(): Unit = {
    val server = new GrpcServer(testDatadir, "localhost", 0)
    serverBinding = Await.result(server.start(), 30.seconds)
    val port = serverBinding.localAddress.getPort
    val clientSettings = GrpcClientSettings
      .connectToServiceAt("localhost", port)
      .withTls(false)
    client = CommonRoutesClient(clientSettings)
  }

  override def afterAll(): Unit = {
    Await.result(client.close(), 10.seconds)
    Await.result(serverBinding.unbind(), 10.seconds)
    Await.result(system.terminate(), 10.seconds)
    FileUtil.deleteTmpDir(testDatadir)
    ()
  }

  "CommonGrpcRoutes" should {
    "getversion" in {
      val response =
        Await.result(client.getVersion(GetVersionRequest()), 10.seconds)
      val expectedVersion = Option(EnvUtil.getVersion)
      assert(response.version == expectedVersion)
    }

    "zipdatadir" in {
      Await.result(
        withTempDir("CommonGrpcRoutesTest") { dir =>
          val fileName = FileUtil.randomDirName
          val dirName = FileUtil.randomDirName
          val target = dir.resolve(dirName).resolve(fileName)

          assert(!Files.exists(target))
          assert(!Files.exists(target.getParent))

          client.zipDataDir(ZipDataDirRequest(path = target.toString)).map {
            _ =>
              assert(Files.exists(target))
          }
        },
        30.seconds
      )
    }
  }
}
