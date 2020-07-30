package org.bitcoins.testkit

import java.nio.file._

import com.typesafe.config._
import org.bitcoins.server.BitcoinSAppConfig
import org.bitcoins.testkit.util.FileUtil

import scala.concurrent.ExecutionContext

object BitcoinSTestAppConfig {

  /** Generates a temp directory with the prefix 'bitcoin-s- */
  def tmpDir(): Path = Files.createTempDirectory("bitcoin-s-")

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
      pgUrl: ProjectType => Option[String],
      config: Config*)(implicit ec: ExecutionContext): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory.parseString {
      """
        |bitcoin-s {
        |  node {
        |     mode = spv
        |  }
        |}
      """.stripMargin
    }

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
      pgUrl: ProjectType => Option[String],
      config: Config*)(implicit ec: ExecutionContext): BitcoinSAppConfig = {
    val overrideConf = ConfigFactory.parseString {
      """
        |bitcoin-s {
        |  node {
        |     mode = neutrino
        |  }
        |}
      """.stripMargin
    }
    BitcoinSAppConfig(
      tmpDir(),
      (overrideConf +: configWithEmbeddedDb(project = None,
                                            pgUrl) +: config): _*)
  }

  sealed trait ProjectType

  object ProjectType {
    case object Wallet extends ProjectType
    case object Node extends ProjectType
    case object Chain extends ProjectType
    case object Test extends ProjectType

    val all = List(Wallet, Node, Chain, Test)
  }

  /** Generates a Typesafe config with DBs set to memory
    * databases for the given project (or all, if no
    * project is given). This configuration can then be
    * given as a override to other configs.
    */
  def configWithEmbeddedDb(
      project: Option[ProjectType],
      pgUrl: ProjectType => Option[String]): Config = {

    def pgConfigForProject(project: ProjectType): String = {
      val name = project.toString().toLowerCase()
      s""" $name.profile = "slick.jdbc.PostgresProfile$$"
         | $name.db {
         |   url = "${pgUrl(project).getOrElse(
        throw new RuntimeException(s"Cannot get db url for $project"))}"
         |   driver = "org.postgresql.Driver"
         |   username = "postgres"
         |   password = ""
         |   connectionPool = disabled
         |   keepAliveConnection = true
         | }""".stripMargin
    }

    def configForProject(project: ProjectType) =
      if (pgUrl(project).isDefined)
        pgConfigForProject(project)
      else
        ""

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
