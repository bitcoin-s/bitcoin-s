package org.bitcoins.testkit

import akka.actor.ActorSystem
import com.typesafe.config._
import org.bitcoins.chain.config.ChainAppConfig
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.dlc.wallet.DLCAppConfig
import org.bitcoins.node.config.NodeAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.db.TestAppConfig
import org.bitcoins.testkit.keymanager.KeyManagerTestUtil
import org.bitcoins.testkit.util.FileUtil
import org.bitcoins.testkit.util.TorUtil.torEnabled
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen.NumberGenerator
import org.bitcoins.wallet.config.WalletAppConfig

import java.nio.file._
import java.util.UUID
import scala.concurrent.ExecutionContext

object BitcoinSTestAppConfig {

  /** Generates a temp directory with the prefix 'bitcoin-s- */
  def tmpDir(): Path = Files.createTempDirectory("bitcoin-s-")

  def genWalletNameConf: Config = {
    val walletNameOpt = if (NumberGenerator.bool.sampleSome) {
      Some(UUID.randomUUID().toString)
    } else None

    walletNameOpt match {
      case Some(walletName) =>
        ConfigFactory.parseString(
          s"bitcoin-s.wallet.walletName = $walletName"
        )

      case None => ConfigFactory.empty()
    }
  }

  def getNeutrinoTestConfig(config: Config*)(implicit
      system: ActorSystem): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory.parseString {
      s"""
         |bitcoin-s {
         |  node {
         |     mode = neutrino
         |     relay = true
         |     use-default-peers = false
         |  }
         |  wallet {
         |    allowExternalDLCAddresses = true
         |  }
         |  proxy.enabled = $torEnabled
         |  tor.enabled = $torEnabled
         |  tor.use-random-ports = false
         |}
      """.stripMargin
    }
    BitcoinSAppConfig(tmpDir(), (overrideConf +: config).toVector)
  }

  def getNeutrinoWithEmbeddedDbTestConfig(
      pgUrl: () => Option[String],
      config: Config*)(implicit system: ActorSystem): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory
      .parseString {
        s"""
           |bitcoin-s {
           |  node {
           |     mode = neutrino
           |     relay = true
           |     use-default-peers = false
           |  }
           |  proxy.enabled = $torEnabled
           |  tor.enabled = $torEnabled
           |  tor.use-random-ports = false  
           |}
      """.stripMargin
      }
      .withFallback(genWalletNameConf)

    BitcoinSAppConfig(
      tmpDir(),
      (overrideConf +: configWithEmbeddedDb(project = None,
                                            pgUrl) +: config).toVector)
  }

  def getMultiPeerNeutrinoWithEmbeddedDbTestConfig(
      pgUrl: () => Option[String],
      config: Config*)(implicit system: ActorSystem): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory
      .parseString {
        s"""
           |bitcoin-s {
           |  node {
           |     mode = neutrino
           |     relay = true
           |     maxConnectedPeers = 8
           |     use-default-peers = false
           |  }
           |  proxy.enabled = $torEnabled
           |  tor.enabled = $torEnabled
           |  tor.use-random-ports = false
           |}
      """.stripMargin
      }
      .withFallback(genWalletNameConf)

    BitcoinSAppConfig(
      tmpDir(),
      (overrideConf +: configWithEmbeddedDb(project = None,
                                            pgUrl) +: config).toVector)
  }

  def getDLCOracleAppConfig(config: Config*)(implicit
      ec: ExecutionContext): DLCOracleAppConfig = {
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

    DLCOracleAppConfig(tmpDir(), (overrideConf +: config).toVector)
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
      (overrideConf +: configWithEmbeddedDb(project = None,
                                            pgUrl) +: config).toVector)
  }

  sealed trait ProjectType

  object ProjectType {
    case object Wallet extends ProjectType
    case object Node extends ProjectType
    case object Chain extends ProjectType
    case object Oracle extends ProjectType
    case object DLC extends ProjectType
    case object Test extends ProjectType
    case class Unknown(projectName: String) extends ProjectType

    val all = List(Wallet, Node, Chain, Oracle, DLC, Test)
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
      val url = pgUrl().getOrElse(
        throw new RuntimeException(s"Cannot get db url for $project"))
      val parts = url.split(":")
      require(parts.size >= 3 && parts(0) == "jdbc",
              s"`$url` must be a valid JDBC URL")
      val str = parts(3)
      val endOfPortStr = str.indexOf('/')
      val (port, _) = str.splitAt(endOfPortStr)
      val projectString = project match {
        case ProjectType.Wallet               => WalletAppConfig.moduleName
        case ProjectType.Chain                => ChainAppConfig.moduleName
        case ProjectType.Node                 => NodeAppConfig.moduleName
        case ProjectType.Oracle               => DLCOracleAppConfig.moduleName
        case ProjectType.DLC                  => DLCAppConfig.moduleName
        case ProjectType.Test                 => TestAppConfig.moduleName
        case ProjectType.Unknown(projectName) => projectName
      }

      val poolName =
        s"bitcoin-s-$projectString-pool-${System.currentTimeMillis()}"

      s""" $projectString.profile = "slick.jdbc.PostgresProfile$$"
         | $projectString.db {
         |   driverName = postgres
         |   name = postgres
         |   url = "$url"
         |   driver = "org.postgresql.Driver"
         |   user = "postgres"
         |   password = "postgres"
         |   poolName = "$poolName"
         |   port = $port
         |   numThreads = 1
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
