package org.bitcoins.explorer.env

import org.bitcoins.crypto.StringFactory

sealed trait ExplorerEnv {
  def baseUri: String
}

object ExplorerEnv extends StringFactory[ExplorerEnv] {

  case object Production extends ExplorerEnv {
    override val baseUri: String = "https://oracle.suredbits.com/v1/"
  }

  case object Test extends ExplorerEnv {
    override val baseUri: String = "https://test.oracle.suredbits.com/v1/"
  }

  /** For local testing purposes */
  case object Local extends ExplorerEnv {
    override val baseUri: String = "http://localhost:9000/v1/"
  }

  val all: Vector[ExplorerEnv] = Vector(Production, Test, Local)

  override def fromString(string: String): ExplorerEnv = {
    val explorerEnvOpt = all.find(_.toString.toLowerCase == string)
    explorerEnvOpt match {
      case Some(env) => env
      case None =>
        sys.error(s"Failed to parse explorer env from str=$string")
    }
  }
}
