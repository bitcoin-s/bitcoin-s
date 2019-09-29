package org.bitcoins.chain.config

import java.nio.file.Files

import akka.actor.ActorSystem
import ch.qos.logback.classic.Level
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config.{MainNet, RegTest, TestNet3}
import org.bitcoins.testkit.chain.ChainUnitTest
import org.bitcoins.testkit.util.FileUtil
import org.scalatest.FutureOutcome

class ChainAppConfigTest extends ChainUnitTest {
  val tempDir = Files.createTempDirectory("bitcoin-s")
  val config = ChainAppConfig(directory = tempDir)

  //if we don't turn off logging here, isInitF a few lines down will
  //produce some nasty error logs since we are testing initialization
  //of the chain project
  val chainAppConfig = appConfig.withOverrides(ConfigFactory.parseString("bitcoin-s.logging.level=OFF"))

  behavior of "ChainAppConfig"

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainFixture(test)

  it must "initialize our chain project" in { _ =>
    val isInitF = chainAppConfig.isInitialized()

    for {
      isInit <- isInitF
      _ = assert(!isInit)
      _ <- chainAppConfig.initialize()
      isInitAgain <- chainAppConfig.isInitialized()
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
    val overriden: ChainAppConfig = config.withOverrides(testnet, mainnet)
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

    val appConfig = ChainAppConfig(directory = tempDir)

    assert(appConfig.datadir == tempDir.resolve("testnet3"))
    assert(appConfig.network == TestNet3)
    assert(appConfig.logLevel == Level.OFF)
    assert(appConfig.p2pLogLevel == Level.WARN)
  }

  override def afterAll: Unit = {

    FileUtil.deleteTmpDir(chainAppConfig.baseDatadir)
  }
}
