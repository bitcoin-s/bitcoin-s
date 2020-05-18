package org.bitcoins.db

import com.typesafe.config.Config
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, BitcoinSUnitTest}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement

import scala.concurrent.ExecutionContext

class DbManagementTest extends BitcoinSAsyncTest {

  def dbConfig(project: ProjectType): Config = {
    BitcoinSTestAppConfig.configWithMemoryDb(Some(project))
  }

  def createChainDbManagement(
      chainAppConfig: ChainAppConfig): ChainDbManagement =
    new ChainDbManagement with JdbcProfileComponent[ChainAppConfig] {
      override val ec: ExecutionContext = system.dispatcher

      override def appConfig: ChainAppConfig = chainAppConfig
    }

  def createWalletDbManagement(
      walletAppConfig: WalletAppConfig): WalletDbManagement =
    new WalletDbManagement with JdbcProfileComponent[WalletAppConfig] {
      override val ec: ExecutionContext = system.dispatcher

      override def appConfig: WalletAppConfig = walletAppConfig
    }

  def createNodeDbManagement(nodeAppConfig: NodeAppConfig): NodeDbManagement =
    new NodeDbManagement with JdbcProfileComponent[NodeAppConfig] {
      override val ec: ExecutionContext = system.dispatcher

      override def appConfig: NodeAppConfig = nodeAppConfig
    }

  it must "run migrations for chain db" in {
    val chainAppConfig = ChainAppConfig(BitcoinSTestAppConfig.tmpDir(),
                                        useLogbackConf = false,
                                        dbConfig(ProjectType.Chain))
    val chainDbManagement = createChainDbManagement(chainAppConfig)
    val result = chainDbManagement.migrate()
    assert(result == 1)
  }

  it must "run migrations for wallet db" in {
    val walletAppConfig = WalletAppConfig(BitcoinSTestAppConfig.tmpDir(),
                                          useLogbackConf = false,
                                          dbConfig(ProjectType.Wallet))
    val walletDbManagement = createWalletDbManagement(walletAppConfig)
    val result = walletDbManagement.migrate()
    assert(result == 4)
  }

  it must "run migrations for node db" in {
    val nodeAppConfig =
      NodeAppConfig(BitcoinSTestAppConfig.tmpDir(),
                    useLogbackConf = false,
                    dbConfig(ProjectType.Node))
    val nodeDbManagement = createNodeDbManagement(nodeAppConfig)
    val result = nodeDbManagement.migrate()
    assert(result == 1)
  }
}
