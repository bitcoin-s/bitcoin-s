package org.bitcoins.server

import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.util.{AkkaUtil, BitcoinSAsyncTest}
import org.scalatest.Assertion

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.reflect.io.Directory

class ServerRunTest extends BitcoinSAsyncTest {

  // Clear log location property
  after {
    System.clearProperty("bitcoins.log.location")
  }

  // Note: on this test passing it will output a stack trace
  // because runMain calls err.printStackTrace() on failure
  it must "throw errors" in {
    val datadir = BitcoinSTestAppConfig.tmpDir()
    val directory = new Directory(datadir.toFile)

    val randPort = RpcUtil.randomPort
    val args = Array("--datadir",
                     datadir.toAbsolutePath.toString,
                     "--rpcport",
                     randPort.toString)

    val main = new BitcoinSServerMain(args, () => system)
    val runMainF = main.start()
    // Use Exception because different errors can occur
    val assertionF: Future[Assertion] = recoverToSucceededIf[Exception] {
      val deleteDirF = for {
        _ <- runMainF
        _ <- AkkaUtil.nonBlockingSleep(2.seconds)
        _ = directory.deleteRecursively()
        _ <- AkkaUtil.nonBlockingSleep(2.seconds)
      } yield ()

      for {
        _ <- runMainF
        _ <- deleteDirF
      } yield ()
    }

    for {
      _ <- assertionF
      _ <- main.stop()
    } yield succeed
  }

}
