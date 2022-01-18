package org.bitcoins.commons.util

import com.typesafe.config.ConfigFactory
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

import java.nio.file.Paths

class ServerArgParserTest extends BitcoinSUnitTest {

  behavior of "ServerArgParser"

  it must "handle no command line flags" in {
    val parser = ServerArgParser(Vector.empty)

    //config must be empty
    assert(parser.toConfig == ConfigFactory.empty())
  }

  it must "handle having all command line args we support" in {
    val datadir = BitcoinSTestAppConfig.tmpDir()
    val datadirString = datadir.toAbsolutePath.toString
    val args = Vector(
      "--rpcport",
      "1234",
      "--rpcbind",
      "my.cool.site.com",
      "--datadir",
      s"${datadirString}",
      "--force-recalc-chainwork",
      "--wsbind",
      "ws.my.cool.site.com",
      "--wsport",
      "5678"
    )
    val parser = ServerArgParser(args)

    val config = parser.toConfig

    val datadirPathConfigKey = s"bitcoin-s.datadir"

    assert(config.hasPath(datadirPathConfigKey))
    assert(config.hasPath(s"bitcoin-s.server.rpcbind"))
    assert(config.hasPath(s"bitcoin-s.server.rpcport"))
    assert(config.hasPath(s"bitcoin-s.chain.force-recalc-chainwork"))
    assert(config.hasPath("bitcoin-s.server.wsport"))
    assert(config.hasPath("bitcoin-s.server.wsbind"))

    val datadirFromConfig = config.getString(datadirPathConfigKey)
    val path = Paths.get(datadirFromConfig)
    assert(path == datadir)
  }
}
