package org.bitcoins.core.protocol

import org.bitcoins.core.config.{ MainNet, NetworkParameters, RegTest, TestNet3 }

/**
 * Represents the HumanReadablePart of a Bech32 address
 * [[https://github.com/bitcoin/bips/blob/master/bip-0173.mediawiki]]
 */
sealed abstract class HumanReadablePart {
  def network: Option[NetworkParameters]
  def bytes: scodec.bits.ByteVector
}

/** Represents the HumanReadablePart for a bitcoin mainnet bech32 address */
case object bc extends HumanReadablePart {
  def network = Some(MainNet)
  def bytes = scodec.bits.ByteVector('b', 'c')
}

/** Represents the HumanReadablePart for a bitcoin testnet bech32 address */
case object tb extends HumanReadablePart {
  def network = Some(TestNet3)
  def bytes = scodec.bits.ByteVector('t', 'b')
}

/** An undefined HRP. This has not been assigned to a network yet */
case class UndefinedHRP(bytes: scodec.bits.ByteVector) extends HumanReadablePart {
  def network = None
}

object HumanReadablePart {

  def apply(str: String) = str match {
    case "bc" => bc
    case "tb" => tb
    case _ => UndefinedHRP(scodec.bits.ByteVector.encodeAscii(str).right.get)
  }

  def apply(network: NetworkParameters): HumanReadablePart = network match {
    case _: MainNet => bc
    case _: TestNet3 | _: RegTest => tb
  }
}
