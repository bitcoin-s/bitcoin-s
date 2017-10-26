package org.bitcoins.core.protocol

import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}

/** Represents the HumanReadablePart of a Bech32 address
  * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki]]
  * */
sealed abstract class HumanReadablePart {
  def network: NetworkParameters
  def bytes: Seq[Byte]
}

/** Represents the HumanReadablePart for a bitcoin mainnet bech32 address */
case object bc extends HumanReadablePart {
  def network = MainNet
  def bytes = Seq('b'.toByte, 'c'.toByte)
}

/** Represents the HumanReadablePart for a bitcoin testnet bech32 address */
case object tb extends HumanReadablePart {
  def network = TestNet3
  def bytes = Seq('t'.toByte, 'b'.toByte)
}

object HumanReadablePart {
  //TODO: try and force a pattern match here so we can check case exhaustiveness
  //i.e if we add a new HumanReadablepart, we get a compiler warning if we didn't add the new case
  def apply(str: String) = str match {
    case "bc" => bc
    case "tb" => tb
  }

  def apply(network: NetworkParameters): HumanReadablePart = network match {
    case _: MainNet => bc
    case _: TestNet3 | _: RegTest => tb
  }
}