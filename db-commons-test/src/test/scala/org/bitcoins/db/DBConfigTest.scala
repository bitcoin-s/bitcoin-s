package org.bitcoins.db

import java.io.{File, IOException}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{
  FileVisitResult,
  Files,
  Path,
  SimpleFileVisitor,
  StandardOpenOption
}

import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.wallet.config.WalletAppConfig
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DBConfigTest extends AnyFlatSpec with Matchers {

  import scala.concurrent.ExecutionContext.Implicits.global

  it should "use sqlite as default database and set its connection pool size to 1" in {
    withTempDir { dataDir =>
      val bytes = Files.readAllBytes(
        new File("db-commons/src/main/resources/db.conf").toPath)
      Files.write(dataDir.resolve("bitcoin-s.conf"),
                  bytes,
                  StandardOpenOption.CREATE_NEW,
                  StandardOpenOption.WRITE)

      val chainConfig = ChainAppConfig(dataDir)
      val slickChainConfig = chainConfig.slickDbConfig
      assert(slickChainConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickChainConfig.config.hasPath("db.numThreads"))
      assert(slickChainConfig.config.getInt("db.numThreads") == 1)
      assert(slickChainConfig.config.getInt("db.queueSize") == 5000)

      val nodeConfig = NodeAppConfig(dataDir)
      val slickNodeConfig = nodeConfig.slickDbConfig
      assert(slickNodeConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickNodeConfig.config.hasPath("db.numThreads"))
      assert(slickNodeConfig.config.getInt("db.numThreads") == 1)
      assert(slickNodeConfig.config.getInt("db.queueSize") == 5000)

      val walletConfig = WalletAppConfig(dataDir)
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
      assert(slickChainConfig.config.getInt("db.numThreads") == 3)
      assert(
        slickChainConfig.config.getString("db.connectionPool") == "disabled")
      assert(slickChainConfig.config.getInt("db.queueSize") == 5000)

      val nodeConfig = NodeAppConfig(dataDir)
      val slickNodeConfig = nodeConfig.slickDbConfig
      assert(slickNodeConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickNodeConfig.config.hasPath("db.numThreads"))
      assert(slickNodeConfig.config.getInt("db.numThreads") == 3)
      assert(
        slickNodeConfig.config.getString("db.connectionPool") == "disabled")
      assert(slickNodeConfig.config.getInt("db.queueSize") == 5000)

      val walletConfig = WalletAppConfig(dataDir)
      val slickWalletConfig = walletConfig.slickDbConfig
      assert(slickWalletConfig.profileName == "slick.jdbc.SQLiteProfile")
      assert(slickWalletConfig.config.hasPath("db.numThreads"))
      assert(slickWalletConfig.config.getInt("db.numThreads") == 3)
      assert(
        slickWalletConfig.config.getString("db.connectionPool") == "disabled")
      assert(slickWalletConfig.config.getInt("db.queueSize") == 5000)
    }
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
            Files.delete(file);
            FileVisitResult.CONTINUE
          }

          override def postVisitDirectory(
              dir: Path,
              exc: IOException): FileVisitResult = {
            Files.delete(dir);
            FileVisitResult.CONTINUE
          }
        }
      )
      ()
    }
  }
}
