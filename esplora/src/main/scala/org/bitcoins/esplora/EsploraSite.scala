package org.bitcoins.esplora

import org.bitcoins.core.config._

sealed abstract class EsploraSite {
  def url: String
  def isTor: Boolean
}

case class BlockstreamEsploraSite(network: BitcoinNetwork) extends EsploraSite {

  override val url: String = network match {
    case MainNet  => "https://blockstream.info/api"
    case TestNet3 => "https://blockstream.info/testnet/api"
    case net @ (RegTest | SigNet) =>
      sys.error(s"Blockstream.info does not support $net")
  }

  override val isTor: Boolean = false
}

case class BlockstreamTorEsploraSite(network: BitcoinNetwork)
    extends EsploraSite {

  override val url: String = network match {
    case MainNet =>
      "http://explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion/api"
    case TestNet3 =>
      "http://explorerzydxu5ecjrkwceayqybizmpjjznk5izmitf2modhcusuqlid.onion/testnet/api"
    case net @ (RegTest | SigNet) =>
      sys.error(s"Blockstream.info does not support $net")
  }

  override val isTor: Boolean = true
}

case class MempoolSpaceEsploraSite(network: BitcoinNetwork)
    extends EsploraSite {

  override val url: String = network match {
    case MainNet  => "https://mempool.space/api"
    case TestNet3 => "https://mempool.space/testnet/api"
    case SigNet   => "https://mempool.space/signet/api"
    case RegTest =>
      sys.error(s"Mempool.space cannot be used for RegTest")
  }

  override val isTor: Boolean = false
}

case class MempoolSpaceTorEsploraSite(network: BitcoinNetwork)
    extends EsploraSite {

  override val url: String = network match {
    case MainNet =>
      "http://mempoolhqx4isw62xs7abwphsq7ldayuidyx2v2oethdhhj6mlo2r6ad.onion/api"
    case TestNet3 =>
      "http://mempoolhqx4isw62xs7abwphsq7ldayuidyx2v2oethdhhj6mlo2r6ad.onion/testnet/api"
    case SigNet =>
      "http://mempoolhqx4isw62xs7abwphsq7ldayuidyx2v2oethdhhj6mlo2r6ad.onion/signet/api"
    case RegTest =>
      sys.error(s"Mempool.space cannot be used for RegTest")
  }

  override val isTor: Boolean = true
}

case class CustomEsploraSite(baseUrl: String, isTor: Boolean)
    extends EsploraSite {

  override val url: String = {
    if (baseUrl.endsWith("/")) baseUrl.init
    else baseUrl
  }
}
