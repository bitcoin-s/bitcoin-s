package org.bitcoins.core.protocol

import org.bitcoins.core.config._
import org.bitcoins.core.util.Bech32HumanReadablePart
import org.bitcoins.crypto.StringFactory

/**
  * Represents the HumanReadablePart of a Bech32 address
  * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki]]
  */
sealed abstract class BtcHumanReadablePart extends Bech32HumanReadablePart {
  def network: BitcoinNetwork
}

object BtcHumanReadablePart extends StringFactory[BtcHumanReadablePart] {

  /** Represents the HumanReadablePart for a bitcoin mainnet bech32 address */
  case object bc extends BtcHumanReadablePart {
    override def network: MainNet.type = MainNet
    override def chars = "bc"
  }

  /** Represents the HumanReadablePart for a bitcoin testnet bech32 address */
  case object tb extends BtcHumanReadablePart {
    override def network: TestNet3.type = TestNet3
    override def chars = "tb"
  }

  /**
    * Represents the HumanReadablePart for a bitcoin regtest bech32 address
    *
    * @see Regtest is not covered in the BIP. See
    *      [[https://github.com/bitcoin/bitcoin/issues/12314 this issue]]
    *      for more context.
    */
  case object bcrt extends BtcHumanReadablePart {
    override def network: RegTest.type = RegTest
    override def chars: String = "bcrt"
  }

  override def fromString(str: String): BtcHumanReadablePart =
    str match {
      case "bc"   => bc
      case "tb"   => tb
      case "bcrt" => bcrt // Bitcoin Core specific
      case _ =>
        throw new IllegalArgumentException(
          s"Could not construct BTC HRP from $str")
    }

  def apply(network: NetworkParameters): BtcHumanReadablePart =
    network match {
      case _: MainNet  => bc
      case _: TestNet3 => tb
      case _: RegTest  => bcrt
    }

  def apply(hrp: Bech32HumanReadablePart): BtcHumanReadablePart =
    BtcHumanReadablePart.fromString(hrp.chars)
}
