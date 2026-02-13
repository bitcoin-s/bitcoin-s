package org.bitcoins.testkit.chain

import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.{BitcoinSTestAppConfig, PostgresTestDatabase}

trait ChainDbUnitTest extends ChainUnitTest with PostgresTestDatabase {

  override def mainnetAppConfig: ChainAppConfig = {
    val memoryDb =
      BitcoinSTestAppConfig.configWithEmbeddedDb(
        Some(ProjectType.Chain),
        postgresOpt
      )
    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val chainConfig: ChainAppConfig =
      BitcoinSTestAppConfig.getNeutrinoTestConfig(mainnetConf).chainConf
    chainConfig.withOverrides(memoryDb)
  }

  override def afterAll(): Unit = {
    super[PostgresTestDatabase].afterAll()
    super[ChainUnitTest].afterAll()
  }
}
