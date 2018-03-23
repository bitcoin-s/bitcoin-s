package org.bitcoins.core.protocol

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }

/**
 * Represents the HumanReadablePart of a Bech32 address
 * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki]]
 */
sealed abstract class HumanReadablePart {
  def network: Option[NetworkParameters]
  def bytes: Seq[Byte]
}

/** Represents the HumanReadablePart for a bitcoin mainnet bech32 address */
case object bc extends HumanReadablePart {
  def network = Some(MainNet)
  def bytes = Seq('b'.toByte, 'c'.toByte)
}

/** Represents the HumanReadablePart for a bitcoin testnet bech32 address */
case object tb extends HumanReadablePart {
  def network = Some(TestNet3)
  def bytes = Seq('t'.toByte, 'b'.toByte)
}

/** An undefined HRP. This has not been assigned to a network yet */
case class UndefinedHRP(bytes: Seq[Byte]) extends HumanReadablePart {
  def network = None
}

object HumanReadablePart {

  def apply(str: String) = str match {
    case "bc" => bc
    case "tb" => tb
    case _    => UndefinedHRP(str.map(_.toByte))
  }

  def apply(network: NetworkParameters): HumanReadablePart = network match {
    case _: MainNet               => bc
    case _: TestNet3 | _: RegTest => tb
  }
}