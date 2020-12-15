package org.bitcoins.testkit

import com.typesafe.config._
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.Implicits.GeneratorOps
import org.bitcoins.testkit.core.gen.{NumberGenerator, StringGenerators}
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import org.bitcoins.testkit.util.FileUtil

import java.nio.file._
import scala.concurrent.ExecutionContext

object BitcoinSTestAppConfig {

  /** Generates a temp directory with the prefix 'bitcoin-s- */
  def tmpDir(): Path = Files.createTempDirectory("bitcoin-s-")

  def genWalletNameConf: Config = {
    val walletNameOpt = if (NumberGenerator.bool.sampleSome) {
      Some(StringGenerators.genNonEmptyString.sampleSome)
    } else None

    walletNameOpt match {
      case Some(walletName) =>
        ConfigFactory.parseString(
          s"bitcoin-s.wallet.walletName = $walletName"
        )

      case None => ConfigFactory.empty()
    }
  }

  /**
    * App configuration suitable for test purposes:
    *
    * 1) Data directory is set to user temp directory
    * 2) Logging is turned down to WARN
    */
  def getSpvTestConfig(config: Config*)(implicit
      ec: ExecutionContext): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory.parseString {
      """
        |bitcoin-s {
        |  node {
        |     mode = spv
        |  }
        |}
      """.stripMargin
    }
    BitcoinSAppConfig(tmpDir(), (overrideConf +: config): _*)
  }

  def getSpvWithEmbeddedDbTestConfig(
      pgUrl: () => Option[String],
      config: Config*)(implicit ec: ExecutionContext): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory
      .parseString {
        """
          |bitcoin-s {
          |  node {
          |     mode = spv
          |  }
          |}
      """.stripMargin
      }
      .withFallback(genWalletNameConf)

    BitcoinSAppConfig(
      tmpDir(),
      (overrideConf +: configWithEmbeddedDb(project = None,
                                            pgUrl) +: config): _*)
  }

  def getNeutrinoTestConfig(config: Config*)(implicit
      ec: ExecutionContext): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory.parseString {
      """
        |bitcoin-s {
        |  node {
        |     mode = neutrino
        |  }
        |}
      """.stripMargin
    }
    BitcoinSAppConfig(tmpDir(), (overrideConf +: config): _*)
  }

  def getNeutrinoWithEmbeddedDbTestConfig(
      pgUrl: () => Option[String],
      config: Config*)(implicit ec: ExecutionContext): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory
      .parseString {
        """
          |bitcoin-s {
          |  node {
          |     mode = neutrino
          |  }
          |}
      """.stripMargin
      }
      .withFallback(genWalletNameConf)

    BitcoinSAppConfig(
      tmpDir(),
      (overrideConf +: configWithEmbeddedDb(project = None,
                                            pgUrl) +: config): _*)
  }

  def getDLCOracleWithEmbeddedDbTestConfig(
      pgUrl: () => Option[String],
      config: Config*)(implicit ec: ExecutionContext): DLCOracleAppConfig = {
    val overrideConf = KeyManagerTestUtil.aesPasswordOpt match {
      case Some(value) =>
        ConfigFactory.parseString {
          s"""
             |bitcoin-s.oracle.aesPassword = $value
      """.stripMargin
        }
      case None =>
        ConfigFactory.empty()
    }

    DLCOracleAppConfig(
      tmpDir(),
      overrideConf +: configWithEmbeddedDb(project = None, pgUrl) +: config: _*)
  }

  sealed trait ProjectType

  object ProjectType {
    case object Wallet extends ProjectType
    case object Node extends ProjectType
    case object Chain extends ProjectType
    case object Oracle extends ProjectType
    case object Test extends ProjectType

    val all = List(Wallet, Node, Chain, Oracle, Test)
  }

  /** Generates a Typesafe config with DBs set to memory
    * databases for the given project (or all, if no
    * project is given). This configuration can then be
    * given as a override to other configs.
    */
  def configWithEmbeddedDb(
      project: Option[ProjectType],
      pgUrl: () => Option[String]): Config = {

    def pgConfigForProject(project: ProjectType): String = {
      val name = project.toString.toLowerCase()
      val url = pgUrl().getOrElse(
        throw new RuntimeException(s"Cannot get db url for $project"))
      val parts = url.split(":")
      require(parts.size >= 3 && parts(0) == "jdbc",
              s"`$url` must be a valid JDBC URL")
      val str = parts(3)
      val endOfPortStr = str.indexOf('/')
      val (port, _) = str.splitAt(endOfPortStr)
      s""" $name.profile = "slick.jdbc.PostgresProfile$$"
         | $name.db {
         |   driverName = postgres
         |   name = postgres
         |   url = "$url"
         |   driver = "org.postgresql.Driver"
         |   user = "postgres"
         |   password = "postgres"
         |   port = $port
         |   numThreads = 10
         |   keepAliveConnection = true
         | }""".stripMargin
    }

    def configForProject(project: ProjectType): String = {
      if (pgUrl().isDefined)
        pgConfigForProject(project)
      else
        ""
    }

    val confStr = project match {
      case None    => ProjectType.all.map(configForProject).mkString("\n")
      case Some(p) => configForProject(p)
    }
    val nestedConfStr = s"""
                           | bitcoin-s {
                           | $confStr
                           | }
                           |""".stripMargin
    ConfigFactory.parseString(nestedConfStr)
  }

  def deleteAppConfig(app: BitcoinSAppConfig): Boolean = {
    FileUtil.deleteTmpDir(app.walletConf.baseDatadir) &&
    FileUtil.deleteTmpDir(app.chainConf.baseDatadir) &&
    FileUtil.deleteTmpDir(app.nodeConf.baseDatadir)
  }
}
