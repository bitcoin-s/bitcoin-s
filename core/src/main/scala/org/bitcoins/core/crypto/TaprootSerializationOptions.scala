package org.bitcoins.core.crypto

import org.bitcoins.core.number.UInt32
import org.bitcoins.crypto.Sha256Digest

/** Options for the taproot signature serialization algorithm as defined
  * in BIP341
  */
case class TaprootSerializationOptions(
    tapLeafHashOpt: Option[Sha256Digest],
    annexHashOpt: Option[Sha256Digest],
    codeSeparatorPosOpt: Option[UInt32]) {
  def haveAnnex: Boolean = annexHashOpt.isDefined

  def codeSeparatorPos: UInt32 = {
    //defaults to UInt32 max if not defined in BIP341
    codeSeparatorPosOpt.getOrElse(UInt32.max)
  }
}

object TaprootSerializationOptions {

  val empty: TaprootSerializationOptions =
    TaprootSerializationOptions(tapLeafHashOpt = None,
                                annexHashOpt = None,
                                codeSeparatorPosOpt = None)
}
