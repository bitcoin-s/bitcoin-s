package org.bitcoins.server

import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Future
import scala.reflect.io.Directory

class ServerRunTest extends BitcoinSAsyncTest {

  // Note: on this test passing it will output a stack trace
  // because runMain calls err.printStackTrace()
  it must "throw errors" in {
    val datadir = BitcoinSTestAppConfig.tmpDir()
    val directory = new Directory(datadir.toFile)

    val args =
      Vector("--datadir", datadir.toAbsolutePath.toString)

    // Use Exception because different errors can occur
    recoverToSucceededIf[Exception] {
      val runMainF = Main.runMain(args)
      val deleteDirF = Future {
        Thread.sleep(2000)
        directory.deleteRecursively()
        Thread.sleep(2000)
      }
      for {
        _ <- runMainF
        _ <- deleteDirF
      } yield ()
    }
  }
}
