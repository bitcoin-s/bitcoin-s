package org.bitcoins.node

import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.core.config.TestNet3
import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.config.MainNet
import java.nio.file.Files

import scala.concurrent.Await

class NodeAppConfigTest extends BitcoinSAsyncTest {
  val tempDir = Files.createTempDirectory("bitcoin-s")

  val config: NodeAppConfig =
    NodeAppConfig(directory = tempDir)

  it must "be overridable" in {
    assert(config.network == RegTest)

    val otherConf = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val withOther: NodeAppConfig = config.withOverrides(otherConf)
    assert(withOther.network == TestNet3)

    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val mainnet: NodeAppConfig = withOther.withOverrides(mainnetConf)
    assert(mainnet.network == MainNet)
    for {
      _ <- withOther.stop()
      _ <- mainnet.stop()
    } yield {
      succeed
    }
  }

  it must "be overridable with multiple levels" in {
    val testnet = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val mainnet = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val overriden: NodeAppConfig = config.withOverrides(testnet, mainnet)
    assert(overriden.network == MainNet)

  }

  it must "have user data directory configuration take precedence" in {

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

    val appConfig = NodeAppConfig(directory = tempDir)

    assert(appConfig.datadir == tempDir.resolve("testnet3"))
    assert(appConfig.network == TestNet3)
  }

  override def afterAll(): Unit = {
    Await.result(config.stop(), akkaTimeout.duration)
    super.afterAll()
  }
}
