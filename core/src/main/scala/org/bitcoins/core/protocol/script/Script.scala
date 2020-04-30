package org.bitcoins.core.protocol.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.crypto.NetworkElement
import scodec.bits.ByteVector

/** This is meant to be a super type for
  * scripts in the bitcoin protocol. This gives us
  * access to the asm representation, and how to serialize the script
  */
abstract class Script extends NetworkElement {

  /**
    * Representation of a script in a parsed assembly format
    * this data structure can be run through the script interpreter to
    * see if a script evaluates to true
    * used to represent the size of the script serialization
    */
  def asm: Seq[ScriptToken]

  /**
    * The byte representation of [[asm]], this does NOT have the bytes
    * for the [[org.bitcoins.core.protocol.CompactSizeUInt]] in the script
    */
  val asmBytes: ByteVector = {
    asm.foldLeft(ByteVector.empty)(_ ++ _.bytes)
  }

  /** The size of the script, this is used for network serialization */
  val compactSizeUInt: CompactSizeUInt = CompactSizeUInt.calc(asmBytes)

  /** The full byte serialization for a script on the network */
  override val bytes: ByteVector = compactSizeUInt.bytes ++ asmBytes

}
