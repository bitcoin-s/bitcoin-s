package org.bitcoins.core.protocol

import org.bitcoins.core.config._
import org.bitcoins.core.util.Bech32HumanReadablePart

import scala.util.{Failure, Success, Try}

/**
  * Represents the HumanReadablePart of a Bech32 address
  * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki]]
  */
sealed abstract class BtcHumanReadablePart extends Bech32HumanReadablePart {
  def network: BitcoinNetwork
}

object BtcHumanReadablePart {

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

  def apply(str: String): Try[BtcHumanReadablePart] = str match {
    case "bc"   => Success(bc)
    case "tb"   => Success(tb)
    case "bcrt" => Success(bcrt) // Bitcoin Core specific
    case _ =>
      Failure(
        new IllegalArgumentException(s"Could not construct BTC HRP from $str"))
  }

  def apply(network: NetworkParameters): BtcHumanReadablePart = network match {
    case _: MainNet  => bc
    case _: TestNet3 => tb
    case _: RegTest  => bcrt
  }

  def apply(hrp: Bech32HumanReadablePart): Try[BtcHumanReadablePart] =
    BtcHumanReadablePart(hrp.chars)
}
