package org.bitcoins.db

import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.config.MainNet
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, FileUtil}
import org.bitcoins.wallet.config.WalletAppConfig

import java.nio.file.*
import scala.concurrent.Future

class DBConfigTest extends BitcoinSAsyncTest {

  it should "use sqlite as default database and set its connection pool size to 1" in {
    withTempDir { dataDir =>
      val chainConfig = ChainAppConfig(dataDir, Vector.empty)
      val nodeConfig = NodeAppConfig(dataDir, Vector.empty)
      val walletConfig = WalletAppConfig(dataDir, Vector.empty)
      for {
        _ <- chainConfig.start()
        _ <- nodeConfig.start()
        _ <- walletConfig.start()
        slickChainConfig = chainConfig.slickDbConfig
        _ = assert(slickChainConfig.profileName == "slick.jdbc.SQLiteProfile")
        _ = assert(slickChainConfig.config.hasPath("db.numThreads"))
        _ = assert(slickChainConfig.config.getInt("db.numThreads") == 1)
        _ = assert(slickChainConfig.config.getInt("db.queueSize") == 5000)

        slickNodeConfig = nodeConfig.slickDbConfig
        _ = assert(slickNodeConfig.profileName == "slick.jdbc.SQLiteProfile")
        _ = assert(slickNodeConfig.config.hasPath("db.numThreads"))
        _ = assert(slickNodeConfig.config.getInt("db.numThreads") == 1)
        _ = assert(slickNodeConfig.config.getInt("db.queueSize") == 5000)

        slickWalletConfig = walletConfig.slickDbConfig
        _ = assert(slickWalletConfig.profileName == "slick.jdbc.SQLiteProfile")
        _ = assert(slickWalletConfig.config.hasPath("db.numThreads"))
        _ = assert(slickWalletConfig.config.getInt("db.numThreads") == 1)
        _ = assert(slickWalletConfig.config.getInt("db.queueSize") == 5000)

        _ <- chainConfig.stop()
        _ <- nodeConfig.stop()
        _ <- walletConfig.stop()
      } yield {
        succeed
      }

    }
  }

  it should "use sqlite as default database and disable connection pool for tests" in {
    withTempDir { dataDir =>
      val chainConfig = ChainAppConfig(dataDir, Vector.empty)
      val nodeConfig = NodeAppConfig(dataDir, Vector.empty)
      val walletConfig = WalletAppConfig(dataDir, Vector.empty)
      for {
        _ <- chainConfig.start()
        _ <- nodeConfig.start()
        _ <- walletConfig.start()
        slickChainConfig = chainConfig.slickDbConfig
        _ = assert(slickChainConfig.profileName == "slick.jdbc.SQLiteProfile")
        _ = assert(slickChainConfig.config.hasPath("db.numThreads"))
        _ = assert(slickChainConfig.config.getInt("db.numThreads") == 1)
        _ = assert(
          slickChainConfig.config.getString("db.connectionPool") == "HikariCP"
        )
        _ = assert(slickChainConfig.config.getInt("db.queueSize") == 5000)

        slickNodeConfig = nodeConfig.slickDbConfig
        _ = assert(slickNodeConfig.profileName == "slick.jdbc.SQLiteProfile")
        _ = assert(slickNodeConfig.config.hasPath("db.numThreads"))
        _ = assert(slickNodeConfig.config.getInt("db.numThreads") == 1)
        _ = assert(
          slickNodeConfig.config.getString("db.connectionPool") == "HikariCP"
        )
        _ = assert(slickNodeConfig.config.getInt("db.queueSize") == 5000)

        slickWalletConfig = walletConfig.slickDbConfig
        _ = assert(slickWalletConfig.profileName == "slick.jdbc.SQLiteProfile")
        _ = assert(slickWalletConfig.config.hasPath("db.numThreads"))
        _ = assert(slickWalletConfig.config.getInt("db.numThreads") == 1)
        _ = assert(
          slickWalletConfig.config.getString("db.connectionPool") == "HikariCP"
        )
        _ = assert(slickWalletConfig.config.getInt("db.queueSize") == 5000)
        _ <- chainConfig.stop()
        _ <- nodeConfig.stop()
        _ <- walletConfig.stop()
      } yield succeed

    }
  }

  it must "override a configuration with a hardcoded value" in {
    val memoryDb =
      BitcoinSTestAppConfig.configWithEmbeddedDb(
        Some(ProjectType.Chain),
        postgresOpt = None
      )
    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val chainConfig: ChainAppConfig = {
      BitcoinSTestAppConfig.getNeutrinoTestConfig(mainnetConf).chainConf
    }

    assert(chainConfig.network == MainNet)

    val mainNetChainAppConfig: ChainAppConfig =
      chainConfig.withOverrides(memoryDb)
    assert(mainNetChainAppConfig.network == MainNet)
  }

  def withTempDir[T](f: Path => Future[T]): Future[T] =
    FileUtil.withTempDir(getClass.getName)(f)
}
