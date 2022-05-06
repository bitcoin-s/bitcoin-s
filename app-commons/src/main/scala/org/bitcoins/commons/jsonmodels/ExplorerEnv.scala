package org.bitcoins.commons.jsonmodels

import org.bitcoins.core.config.{
  BitcoinNetwork,
  MainNet,
  RegTest,
  SigNet,
  TestNet3
}
import org.bitcoins.crypto.StringFactory

sealed trait ExplorerEnv {
  def siteUrl: String
  def baseUri: String
  def torUrl: String
  def torBaseUri: String
}

object ExplorerEnv extends StringFactory[ExplorerEnv] {

  case object Production extends ExplorerEnv {
    override val siteUrl: String = "https://oracle.suredbits.com/"
    override val baseUri: String = s"${siteUrl}v2/"

    override val torUrl: String =
      "http://oraclez4zobtsekasdz5aq3pgw75cwrso5dbckfz5wrfexfhv7755oid.onion/"
    override val torBaseUri: String = s"${torUrl}v2/"
  }

  case object Test extends ExplorerEnv {
    override val siteUrl: String = "https://test.oracle.suredbits.com/"
    override val baseUri: String = s"${siteUrl}v2/"

    override val torUrl: String =
      "http://ps2u26igy7p4ykbniaog4scqn6sjcsbcwglnuv4gq6amtchrzurxy4ad.onion/"
    override val torBaseUri: String = s"${torUrl}v2/"
  }

  /** For local testing purposes */
  case object Local extends ExplorerEnv {
    override val siteUrl: String = "http://localhost:9000/"
    override val baseUri: String = s"${siteUrl}v2/"
    override val torUrl: String = siteUrl
    override val torBaseUri: String = baseUri
  }

  val all: Vector[ExplorerEnv] = Vector(Production, Test, Local)

  override def fromString(string: String): ExplorerEnv = {
    val explorerEnvOpt = all.find(_.toString.toLowerCase == string.toLowerCase)
    explorerEnvOpt match {
      case Some(env) => env
      case None =>
        sys.error(s"Failed to parse explorer env from str=$string")
    }
  }

  def fromBitcoinNetwork(network: BitcoinNetwork): ExplorerEnv = {
    network match {
      case MainNet                     => Production
      case TestNet3 | RegTest | SigNet => Test
    }
  }
}
