package org.bitcoins.db

import com.typesafe.config.Config
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement

class DbManagementTest extends BitcoinSUnitTest {
  def dbConfig(project: ProjectType): Config = {
    BitcoinSTestAppConfig.configWithMemoryDb(Some(project))
  }
  it must "run migrations for chain db" in {
    val chainAppConfig = ChainAppConfig(BitcoinSTestAppConfig.tmpDir(),
      dbConfig(ProjectType.Chain))
    val result = ChainDbManagement.migrate(chainAppConfig)
    assert(result == 1)
  }

  it must "run migrations for wallet db" in {
    val walletAppConfig = WalletAppConfig(BitcoinSTestAppConfig.tmpDir(),
      dbConfig(ProjectType.Wallet))
    val result = WalletDbManagement.migrate(walletAppConfig)
    assert(result == 2)
  }


  it must "run migrations for node db" in {
    val nodeAppConfig = NodeAppConfig(BitcoinSTestAppConfig.tmpDir(),
      dbConfig(ProjectType.Node))
    val result = NodeDbManagement.migrate(nodeAppConfig)
    assert(result == 1)
  }
}
