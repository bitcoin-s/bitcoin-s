package org.bitcoins.core.protocol.script
import org.bitcoins.crypto.{Factory, NetworkElement}
import scodec.bits.ByteVector

/** A type to represent the Taproot annex as per BIP341
  * [[https://github.com/bitcoin/bips/blob/master/bip-0341.mediawiki#cite_note-5]]
  */
sealed trait Annex extends NetworkElement {
  require(bytes.head == TaprootScriptPath.annex,
          s"Annex did not have correct first byte, got=${bytes.head}")
}

object Annex extends Factory[Annex] {
  case class Unknown(bytes: ByteVector) extends Annex
  override def fromBytes(bytes: ByteVector): Annex = Unknown(bytes)
}
