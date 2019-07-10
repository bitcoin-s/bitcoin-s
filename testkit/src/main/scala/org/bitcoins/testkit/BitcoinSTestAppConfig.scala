package org.bitcoins.testkit

import org.bitcoins.server.BitcoinSAppConfig
import com.typesafe.config._
import java.nio.file._

object BitcoinSTestAppConfig {

  /**
    * App configuration suitable for test purposes:
    *
    * 1) Data directory is set to user temp directory
    */
  def getTestConfig(config: Config*): BitcoinSAppConfig = {
    val tmpDir = Files.createTempDirectory("bitcoin-s-")
    val confStr = s"""
    | bitcoin-s {
    |   datadir = $tmpDir
    | }
    |
    |""".stripMargin
    val conf = ConfigFactory.parseString(confStr)
    val allConfs = conf +: config
    BitcoinSAppConfig(allConfs: _*)
  }

  sealed trait ProjectType

  object ProjectType {
    case object Wallet extends ProjectType
    case object Node extends ProjectType
    case object Chain extends ProjectType

    val all = List(Wallet, Node, Chain)
  }

  /** Generates a Typesafe config with DBs set to memory
    * databases for the given project (or all, if no
    * project is given). This configuration can then be
    * given as a override to other configs.
    */
  def configWithMemoryDb(project: Option[ProjectType]): Config = {
    def memConfigForProject(project: ProjectType): String = {
      val name = project.toString().toLowerCase()
      s"""
        | $name.db {
        |   url = "jdbc:sqlite:file:$name.db:?mode=memory&cache=shared"
        |   connectionPool = disabled
        |   keepAliveConnection = true
        | }
        |""".stripMargin
    }

    val confStr = project match {
      case None    => ProjectType.all.map(memConfigForProject).mkString("\n")
      case Some(p) => memConfigForProject(p)
    }
    val nestedConfStr = s"""
    | bitcoin-s {
    | $confStr
    | }
    |""".stripMargin
    ConfigFactory.parseString(nestedConfStr)
  }
}
