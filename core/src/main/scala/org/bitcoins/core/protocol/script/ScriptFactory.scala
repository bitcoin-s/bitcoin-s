package org.bitcoins.core.protocol.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.util.{BitcoinSUtil, BitcoinScriptUtil, Factory}
import scodec.bits.ByteVector

/**
  * Created by chris on 12/9/16.
  */
trait ScriptFactory[T <: Script] extends Factory[T] {

  /** Builds a script from the given asm with the given constructor if the invariant holds true, else throws an error */
  def buildScript(
      asm: Vector[ScriptToken],
      constructor: Vector[ScriptToken] => T,
      invariant: Seq[ScriptToken] => Boolean,
      errorMsg: String): T = {
    if (invariant(asm)) {
      constructor(asm)
    } else throw new IllegalArgumentException(errorMsg)
  }

  /** Creates a T from the given [[org.bitcoins.core.script.constant.ScriptToken ScriptToken]]s */
  def fromAsm(asm: Seq[ScriptToken]): T

  def fromBytes(bytes: ByteVector): T = {
    BitcoinScriptUtil.parseScript(bytes = bytes, f = fromAsm)
  }

  /**
    * Scripts are serialized with a [[org.bitcoins.core.protocol.CompactSizeUInt CompactSizeUInt]] at the beginning
    * to indicate how long the Script is. This construct assumes the
    * [[org.bitcoins.core.protocol.CompactSizeUInt CompactSizeUInt]]
    * is NOT passed into the constructor. Only the actual Script program bytes.
    */
  def fromAsmBytes(bytes: ByteVector): T = {
    val cmpct = CompactSizeUInt.calc(bytes)
    val fullBytes = cmpct.bytes ++ bytes
    fromBytes(fullBytes)
  }

  /**
    * Scripts are serialized with a [[org.bitcoins.core.protocol.CompactSizeUInt CompactSizeUInt]] at the beginning
    * to indicate how long the [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]] is
    * . This construct assumes the [[org.bitcoins.core.protocol.CompactSizeUInt CompactSizeUInt]]
    * is NOT passed into the constructor. Only the actual Script program hex is.
    */
  def fromAsmHex(hex: String): T = {
    fromAsmBytes(BitcoinSUtil.decodeHex(hex))
  }
}
