package org.bitcoins.server

import java.nio.file._

import com.typesafe.config.ConfigFactory
import org.bitcoins.core.config._
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.reflect.io.Directory

class BitcoindRpcAppConfigTest extends BitcoinSAsyncTest {

  val tempDir: Path = Files.createTempDirectory("bitcoin-s")

  val config: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig(directory = tempDir)

  override def afterAll(): Unit = {
    super.afterAll()
    new Directory(tempDir.toFile).deleteRecursively()
  }

  it must "be overridable" in {
    assert(config.rpcPort == RegTest.rpcPort)

    val otherConf =
      ConfigFactory.parseString("bitcoin-s.bitcoind-rpc.rpcport = 5555")
    val withOther = config.withOverrides(otherConf)
    assert(withOther.rpcPort == 5555)

    val mainnetConf = ConfigFactory.parseString(
      s"bitcoin-s.bitcoind-rpc.rpcport = ${MainNet.rpcPort}")
    val mainnet = withOther.withOverrides(mainnetConf)
    assert(mainnet.rpcPort == MainNet.rpcPort)
  }

  it should "not matter how the overrides are passed in" in {
    val overrider =
      ConfigFactory.parseString(s"bitcoin-s.bitcoind-rpc.rpcport = 5555")

    val throughConstructor = BitcoindRpcAppConfig(tempDir, overrider)
    val throughWithOverrides = config.withOverrides(overrider)
    assert(throughWithOverrides.rpcPort == 5555)
    assert(throughWithOverrides.rpcPort == throughConstructor.rpcPort)

    assert(throughWithOverrides.datadir == throughConstructor.datadir)

  }

  it must "be overridable without screwing up other options" in {
    val otherConf =
      ConfigFactory.parseString(s"bitcoin-s.bitcoind-rpc.rpcport = 5555")

    val thirdConf =
      ConfigFactory.parseString(s"bitcoin-s.bitcoind-rpc.rpcport = 5554")

    val overridden = config.withOverrides(otherConf)

    val twiceOverridden = overridden.withOverrides(thirdConf)

    assert(overridden.rpcPort == 5555)
    assert(twiceOverridden.rpcPort == 5554)

    assert(config.datadir == overridden.datadir)
    assert(twiceOverridden.datadir == overridden.datadir)
  }

  it must "be overridable with multiple levels" in {
    val testnet = ConfigFactory.parseString("bitcoin-s.network = testnet3")
    val mainnet = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val overriden: BitcoindRpcAppConfig = config.withOverrides(testnet, mainnet)
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

    val appConfig = BitcoindRpcAppConfig(directory = tempDir)

    assert(appConfig.datadir == tempDir.resolve("testnet3"))
    assert(appConfig.network == TestNet3)
  }
}
