package org.bitcoins.core.crypto.bip44
import org.bitcoins.core.crypto.bip32.{BIP32Node, BIP32Path}

/**
  * Represents a
  * [[https://github.com/bitcoin/bips/blob/master/bip-0044.mediawiki#change BIP44]]
  * coin type.
  */
sealed abstract class BIP44Coin extends BIP32Path {
  override def path: Vector[BIP32Node] =
    Vector(BIP44Path.purposeChild, BIP32Node(toInt, hardened = true))

  def toInt: Int

  def toAccount(index: Int): BIP44Account = BIP44Account(this, index)
}

/**
  * @see [[https://github.com/satoshilabs/slips/blob/master/slip-0044.md SLIP-0044]]
  *     central registry of coin types
  */
object BIP44Coin {

  final case object Bitcoin extends BIP44Coin {
    override val toInt: Int = 0
  }

  /**
    * All coins
    */
  final case object Testnet extends BIP44Coin {
    override val toInt: Int = 1
  }

  def fromInt(int: Int): BIP44Coin =
    int match {
      case Bitcoin.toInt => Bitcoin
      case Testnet.toInt => Testnet
      case _: Int =>
        throw new IllegalArgumentException(s"$int is not valid coin type!")
    }
}
