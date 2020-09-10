package org.bitcoins.db

import com.typesafe.config.Config
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.chain.db.ChainDbManagement
import org.bitcoins.db.DatabaseDriver._
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.node.db.NodeDbManagement
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkit.{BitcoinSTestAppConfig, EmbeddedPg}
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
                                        dbConfig(ProjectType.Chain))
    val chainDbManagement = createChainDbManagement(chainAppConfig)
    val result = chainDbManagement.migrate()
    val expected = chainAppConfig.driver match {
      case SQLite     => 5
      case PostgreSQL => 4
    }
    assert(result == expected)
  }

  it must "run migrations for wallet db" in {
    val walletAppConfig = WalletAppConfig(BitcoinSTestAppConfig.tmpDir(),
                                          dbConfig(ProjectType.Wallet))
    val walletDbManagement = createWalletDbManagement(walletAppConfig)
    val result = walletDbManagement.migrate()
    val expected = walletAppConfig.driver match {
      case SQLite     => 8
      case PostgreSQL => 6
    }
    assert(result == expected)
  }

  it must "run migrations for node db" in {
    val nodeAppConfig =
      NodeAppConfig(BitcoinSTestAppConfig.tmpDir(), dbConfig(ProjectType.Node))
    val nodeDbManagement = createNodeDbManagement(nodeAppConfig)
    val result = nodeDbManagement.migrate()
    val expected = nodeAppConfig.driver match {
      case SQLite     => 2
      case PostgreSQL => 2
    }
    assert(result == expected)
  }
}
