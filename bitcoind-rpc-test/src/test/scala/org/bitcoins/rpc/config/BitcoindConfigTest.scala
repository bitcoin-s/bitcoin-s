package org.bitcoins.rpc.config

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.config.MainNet
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest

class BitcoindConfigTest extends BitcoinSUnitTest {

  def tmpDir = BitcoindRpcTestUtil.tmpDir()
  it must "have to/fromString symmetry" in {
    val conf = BitcoindRpcTestUtil.standardConfig
    val confStr = conf.toWriteableString
    val otherConf = BitcoindConfig(confStr, tmpDir)
    val otherConfStr = otherConf.toWriteableString
    assert(confStr == otherConfStr)
  }

  it must "parse networks" in {
    val conf = BitcoindConfig("""
                                |regtest=1
        """.stripMargin,
                              tmpDir)
    assert(conf.network == RegTest)
  }

  it must "prioritize a prefixed option" in {
    val confStr = """
                    |regtest=1
                    |
                    |rpcport=2000
                    |regtest.rpcport=3000
                    |
                    |[regtest]
                    |rpcport=4000
    """.stripMargin.split("\n")

    val conf = BitcoindConfig(confStr, tmpDir)
    assert(conf.rpcport == 3000)
    assert(conf.network == RegTest)
  }

  it must "avoid to get prefixed options in section headers" in {
    val confStr = """
                    |regtest=1
                    |
                    |rpcport=2000
                    |
                    |[regtest]
                    |rpcport=4000
                    |
                    |regtest.rpcport=3000
    """.stripMargin.split("\n")

    val conf = BitcoindConfig(confStr, tmpDir)
    assert(conf.rpcport == 4000)
    assert(conf.network == RegTest)
  }

  it must "avoid getting options for the wrong network" in {
    val confStr = """
                    |testnet=1
                    |
                    |[regtest]
                    |rpcport=4000
                    |
                    |regtest.rpcport=3000
    """.stripMargin.split("\n")

    val conf = BitcoindConfig(confStr, tmpDir)
    assert(conf.rpcport == TestNet3.rpcPort)
    assert(conf.network == TestNet3)
  }

  it must "get options with multiple header sections" in {
    val confStr = """
                    |testnet=1
                    |
                    |[test]
                    |rpcuser=username
                    |rpcport=3000
                    |
                    |[regtest]
                    |rpcport=4000
                    |
                    |[main]
                    |rpcport=1000
    """.stripMargin.split("\n")

    val conf = BitcoindConfig(confStr, tmpDir)
    assert(conf.rpcport == 3000)
    assert(conf.network == TestNet3)
    assert(conf.username.contains("username"))
  }

  it must "fallback to default values" in {
    val conf = BitcoindConfig.empty
    assert(conf.network == MainNet)
    assert(conf.rpcport == MainNet.rpcPort)
    assert(conf.password.isEmpty)
    assert(conf.username.isEmpty)
  }

  it must "get options with multiple header sections when our section is the last" in {
    val confStr = """
                    |regtest=1
                    |
                    |[test]
                    |rpcport=3000
                    |
                    |[main]
                    |rpcport=1000

                    |[regtest]
                    |rpcport=4000
                    |rpcuser=username
    """.stripMargin.split("\n")

    val conf = BitcoindConfig(confStr, tmpDir)
    assert(conf.rpcport == 4000)
    assert(conf.network == RegTest)
    assert(conf.username.contains("username"))

  }

  it must "have a default config in test utils" in {
    val conf = BitcoindRpcTestUtil.standardConfig
    assert(conf.username.isDefined)
    assert(conf.password.isDefined)
    assert(conf.zmqpubhashblock.isDefined)
    assert(conf.zmqpubhashtx.isDefined)
    assert(conf.zmqpubrawblock.isDefined)
    assert(conf.zmqpubrawtx.isDefined)
    assert {
      conf.rpcUri // cal by name
      true
    }

    assert {
      conf.uri // cal by name
      true
    }
  }
}
