package org.bitcoins.server.grpc

import org.bitcoins.core.util.EnvUtil
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.bitcoins.testkit.util.FileUtil

import java.nio.file.Files
import scala.concurrent.ExecutionContext

class CommonGrpcRoutesTest extends ServerGrpcFixture {

  implicit val ec: ExecutionContext = system.dispatcher

  behavior of "CommonGrpcRoutes"

  it must "getversion" in { case (client, server) =>
    val responseF =
      client.getVersion(GetVersionRequest())
    val expectedVersion = Option(EnvUtil.getVersion)
    responseF.map { response =>
      assert(response.version == expectedVersion)
    }
  }

  it must "zipdatadir" in { case (client, server) =>
    val fileName = FileUtil.randomDirName
    val dirName = FileUtil.randomDirName
    val dir = FileUtil.tmpDir().toPath
    val target = dir.resolve(dirName).resolve(fileName)

    assert(!Files.exists(target))
    assert(!Files.exists(target.getParent))

    client.zipDataDir(ZipDataDirRequest(path = target.toString)).map { _ =>
      assert(Files.exists(target))
    }
  }
}
