package org.bitcoins.db

import com.typesafe.config.ConfigFactory
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.core.config.MainNet
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.BitcoinSTestAppConfig.ProjectType
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.wallet.config.WalletAppConfig

import java.io.{File, IOException}
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import scala.reflect.io.Directory

class DBConfigTest extends BitcoinSAsyncTest {

  it should "use sqlite as default database and set its connection pool size to 1" in {
    withTempDir { dataDir =>
      val bytes = Files.readAllBytes(
        new File("db-commons/src/main/resources/reference.conf").toPath)
      Files.write(dataDir.resolve("bitcoin-s.conf"),
                  bytes,
                  StandardOpenOption.CREATE_NEW,
                  StandardOpenOption.WRITE)

      val chainConfig = ChainAppConfig(dataDir)
      val nodeConfig = NodeAppConfig(dataDir)
      val walletConfig = WalletAppConfig(dataDir)

      val slickChainConfig = chainConfig.slickDbConfig
      assert(slickChainConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickChainConfig.config.hasPath("db.numThreads"))
      assert(slickChainConfig.config.getInt("db.numThreads") == 1)
      assert(slickChainConfig.config.getInt("db.queueSize") == 5000)

      val slickNodeConfig = nodeConfig.slickDbConfig
      assert(slickNodeConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickNodeConfig.config.hasPath("db.numThreads"))
      assert(slickNodeConfig.config.getInt("db.numThreads") == 1)
      assert(slickNodeConfig.config.getInt("db.queueSize") == 5000)

      val slickWalletConfig = walletConfig.slickDbConfig
      assert(slickWalletConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickWalletConfig.config.hasPath("db.numThreads"))
      assert(slickWalletConfig.config.getInt("db.numThreads") == 1)
      assert(slickWalletConfig.config.getInt("db.queueSize") == 5000)
    }
  }

  it should "use sqlite as default database and disable connection pool for tests" in {
    withTempDir { dataDir =>
      val chainConfig = ChainAppConfig(dataDir)
      val slickChainConfig = chainConfig.slickDbConfig
      assert(slickChainConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickChainConfig.config.hasPath("db.numThreads"))
      assert(slickChainConfig.config.getInt("db.numThreads") == 1)
      assert(
        slickChainConfig.config.getString("db.connectionPool") == "disabled")
      assert(slickChainConfig.config.getInt("db.queueSize") == 5000)

      val nodeConfig = NodeAppConfig(dataDir)
      val slickNodeConfig = nodeConfig.slickDbConfig
      assert(slickNodeConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickNodeConfig.config.hasPath("db.numThreads"))
      assert(slickNodeConfig.config.getInt("db.numThreads") == 1)
      assert(
        slickNodeConfig.config.getString("db.connectionPool") == "disabled")
      assert(slickNodeConfig.config.getInt("db.queueSize") == 5000)

      val walletConfig = WalletAppConfig(dataDir)
      val slickWalletConfig = walletConfig.slickDbConfig
      assert(slickWalletConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickWalletConfig.config.hasPath("db.numThreads"))
      assert(slickWalletConfig.config.getInt("db.numThreads") == 1)
      assert(
        slickWalletConfig.config.getString("db.connectionPool") == "disabled")
      assert(slickWalletConfig.config.getInt("db.queueSize") == 5000)
    }
  }

  it must "override a configuration with a hardcoded value" in {
    val memoryDb =
      BitcoinSTestAppConfig.configWithEmbeddedDb(Some(ProjectType.Chain),
                                                 () => None)
    val mainnetConf = ConfigFactory.parseString("bitcoin-s.network = mainnet")
    val chainConfig: ChainAppConfig = {
      BitcoinSTestAppConfig.getSpvTestConfig(mainnetConf)
    }

    assert(chainConfig.network == MainNet)

    val mainNetChainAppConfig: ChainAppConfig =
      chainConfig.withOverrides(memoryDb)
    assert(mainNetChainAppConfig.network == MainNet)
  }

  def withTempDir[T](f: Path => T): T = {
    val dir = Files.createTempDirectory(getClass.getName)
    try {
      f(dir)
    } finally {
      Files.walkFileTree(
        dir,
        new SimpleFileVisitor[Path] {
          override def visitFile(
              file: Path,
              attrs: BasicFileAttributes): FileVisitResult = {
            val directory = new Directory(file.toFile)
            directory.deleteRecursively()
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(
              dir: Path,
              exc: IOException): FileVisitResult = {
            val directory = new Directory(dir.toFile)
            directory.deleteRecursively()
            FileVisitResult.CONTINUE
          }
        }
      )
      ()
    }
  }
}
