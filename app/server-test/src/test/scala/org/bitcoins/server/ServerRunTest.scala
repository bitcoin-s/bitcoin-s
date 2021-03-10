package org.bitcoins.server

import java.nio.file._
import org.bitcoins.rpc.client.common.BitcoindVersion
import org.bitcoins.rpc.util.RpcUtil
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.BitcoinSFixture
import org.bitcoins.testkit.util.{AkkaUtil, BitcoinSAsyncTest}

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

    // Use Exception because different errors can occur
    recoverToSucceededIf[Exception] {
      val runMainF = new BitcoinSServerMain(args).startup
      val deleteDirF = for {
        _ <- AkkaUtil.nonBlockingSleep(2.seconds)
        _ = directory.deleteRecursively()
        _ <- AkkaUtil.nonBlockingSleep(2.seconds)
      } yield ()

      for {
        _ <- runMainF
        _ <- deleteDirF
      } yield ()
    }
  }

  it must "start up and log to the correct location" in {
    val datadir = BitcoinSTestAppConfig.tmpDir()
    val directory = new Directory(datadir.toFile)
    val confFile = datadir.resolve("bitcoin-s.conf")

    for {
      bitcoind <-
        BitcoinSFixture.createBitcoindWithFunds(Some(BitcoindVersion.V21))

      // Make it so we connect to the correct bitcoind
      port = bitcoind.instance.uri.getPort
      confStr = s"""bitcoin-s.node.peers = ["localhost:$port"]"""
      _ = Files.write(confFile, confStr.getBytes)

      // Add config options
      randPort = RpcUtil.randomPort
      args = Array("--datadir",
                   datadir.toAbsolutePath.toString,
                   "--rpcport",
                   randPort.toString)

      // Start the server in a separate thread
      runnable = new Runnable {
        override def run(): Unit = new BitcoinSServerMain(args).run()
      }
      thread = new Thread(runnable)
      _ = thread.start()

      // Wait for the server to have successfully started up
      _ <- AkkaUtil.nonBlockingSleep(1.second)
      binding <- BitcoinSServer.startedF

      // Stop the server
      _ <- bitcoind.stop()
      _ <- binding.terminate(5.seconds)
      _ = thread.interrupt()
    } yield {
      // Cleanup
      directory.deleteRecursively()

      val expectedDir = datadir.resolve("regtest")

      // Check the log location was correctly set
      assert(
        System.getProperty("bitcoins.log.location") == expectedDir.toString)
    }
  }
}
