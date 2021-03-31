package org.bitcoins.explorer.env

sealed trait ExplorerEnv {
  def baseUri: String
}

object ExplorerEnv {

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
}
