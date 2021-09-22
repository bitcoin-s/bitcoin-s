package org.bitcoins.server

import com.typesafe.config.ConfigFactory
import org.bitcoins.commons.util.ServerArgParser
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
    //custom configuration to make peers empty
    //this should cause an exception in startBitcoinSBackend()
    val noPeersConfig =
      ConfigFactory.parseString(s"""bitcoin-s.node.peers=[]""")
    implicit val config =
      BitcoinSTestAppConfig.getNeutrinoTestConfig(noPeersConfig)
    val datadir = config.chainConf.datadir
    val directory = new Directory(datadir.toFile)

    val invalidPort = -1
    val args = Vector("--datadir",
                      datadir.toAbsolutePath.toString,
                      "--rpcport",
                      invalidPort.toString)

    val serverArgParser = ServerArgParser(args)
    val main = new BitcoinSServerMain(serverArgParser)
    val runMainF = main.start()
    // Use Exception because different errors can occur
    val assertionF: Future[Assertion] = recoverToSucceededIf[Exception] {
      val deleteDirF = for {
        _ <- runMainF
        _ <- AkkaUtil.nonBlockingSleep(2.seconds)
        _ = directory.deleteRecursively()
        _ <- AkkaUtil.nonBlockingSleep(5.seconds)
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
