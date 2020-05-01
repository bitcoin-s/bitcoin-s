package org.bitcoins.db

import com.typesafe.config.Config
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, BitcoinSUnitTest}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.db.WalletDbManagement

import scala.concurrent.ExecutionContext

class DbManagementTest extends BitcoinSAsyncTest with EmbeddedPg {

  def dbConfig(project: ProjectType): Config = {
    BitcoinSTestAppConfig.configWithEmbeddedDb(Some(project), pgUrl)
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
    val expected = if (chainAppConfig.driverName == "postgresql") 2 else 3
    assert(result == expected)
  }

  it must "run migrations for wallet db" in {
    val walletAppConfig = WalletAppConfig(BitcoinSTestAppConfig.tmpDir(),
                                          useLogbackConf = false,
                                          dbConfig(ProjectType.Wallet))
    val walletDbManagement = createWalletDbManagement(walletAppConfig)
    val result = walletDbManagement.migrate()
    val expected = if (walletAppConfig.driverName == "postgresql") 4 else 7
    assert(result == expected)
  }

  it must "run migrations for node db" in {
    val nodeAppConfig =
      NodeAppConfig(BitcoinSTestAppConfig.tmpDir(),
                    useLogbackConf = false,
                    dbConfig(ProjectType.Node))
    val nodeDbManagement = createNodeDbManagement(nodeAppConfig)
    val result = nodeDbManagement.migrate()
    assert(result == 2)
  }
}
