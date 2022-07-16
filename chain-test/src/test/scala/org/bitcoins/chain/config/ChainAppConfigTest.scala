package org.bitcoins.chain.config

import java.nio.file.Files
import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.{ChainCallbacks, OnBlockHeaderConnected}
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.core.protocol.blockchain.BlockHeader
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome

import scala.concurrent.{Await, Future}

class ChainAppConfigTest extends ChainUnitTest {
  val tempDir = Files.createTempDirectory("bitcoin-s")
  val config = ChainAppConfig(baseDatadir = tempDir, Vector.empty)

  //if we don't turn off logging here, isInitF a few lines down will
  //produce some nasty error logs since we are testing initialization
  //of the chain project
  val chainAppConfig = cachedChainConf.withOverrides(
    ConfigFactory.parseString("bitcoin-s.logging.level=OFF"))

  behavior of "ChainAppConfig"

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainFixture(test)

  it must "initialize our chain project" in { _ =>
    val isInitF = chainAppConfig.isStarted()

    for {
      isInit <- isInitF
      _ = assert(!isInit)
      _ <- chainAppConfig.start()
      isInitAgain <- chainAppConfig.isStarted()
    } yield assert(isInitAgain)
  }

  it must "be overridable" in { _ =>
    assert(config.network == RegTest)

    val otherConf = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val withOther: ChainAppConfig = config.withOverrides(otherConf)
    assert(withOther.network == TestNet3)

    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val mainnet: ChainAppConfig = withOther.withOverrides(mainnetConf)
    assert(mainnet.network == MainNet)
  }

  it must "be overridable with multiple levels" in { _ =>
    val testnet = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val mainnet = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val overriden: ChainAppConfig =
      config.withOverrides(Vector(testnet, mainnet))
    assert(overriden.network == MainNet)

  }

  it must "have user data directory configuration take precedence" in { _ =>
    val tempDir = Files.createTempDirectory("bitcoin-s")
    val tempFile = Files.createFile(tempDir.resolve("bitcoin-s.conf"))
    val confStr = """
                    | bitcoin-s {
                    |   network = testnet3
                    |   
                    |   logging {
                    |     level = off
                    |
                    |     p2p = warn
                    |   }
                    | }
    """.stripMargin
    val _ = Files.write(tempFile, confStr.getBytes())

    val appConfig = ChainAppConfig(baseDatadir = tempDir, Vector.empty)

    assert(appConfig.datadir == tempDir.resolve("testnet3"))
    assert(appConfig.network == TestNet3)
  }

  it must "add a callback and then remove the callback" in { _ =>
    val tempDir = Files.createTempDirectory("bitcoin-s")
    val appConfig = ChainAppConfig(baseDatadir = tempDir, Vector.empty)
    assert(appConfig.isCallbackEmpty)

    val dummyCallback: OnBlockHeaderConnected = {
      case _: Vector[(Int, BlockHeader)] =>
        Future.unit
    }
    val printlnCallback = ChainCallbacks.onBlockHeaderConnected(dummyCallback)

    appConfig.addCallbacks(printlnCallback)
    assert(!appConfig.isCallbackEmpty)

    //clear the callback
    appConfig.clearCallbacks()
    assert(appConfig.isCallbackEmpty)
  }

  override def afterAll(): Unit = {
    FileUtil.deleteTmpDir(chainAppConfig.baseDatadir)
    val stopF = for {
      _ <- config.stop()
      _ <- chainAppConfig.stop()
    } yield ()
    Await.result(stopF, akkaTimeout.duration)
    super.afterAll()
  }
}
