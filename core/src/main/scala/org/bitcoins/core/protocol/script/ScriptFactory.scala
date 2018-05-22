package org.bitcoins.core.protocol.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util.{ BitcoinSUtil, Factory }

/**
 * Created by chris on 12/9/16.
 */
trait ScriptFactory[T] extends Factory[T] {

  /** Builds a script from the given asm with the given constructor if the invariant holds true, else throws an error */
  def buildScript(asm: Seq[ScriptToken], constructor: scodec.bits.ByteVector => T,
    invariant: Seq[ScriptToken] => Boolean, errorMsg: String): T = {
    if (invariant(asm)) {
      val asmBytes = BitcoinSUtil.toByteVector(asm)
      val compactSizeUInt = CompactSizeUInt.calc(asmBytes)
      constructor(compactSizeUInt.bytes ++ asmBytes)
    } else throw new IllegalArgumentException(errorMsg)
  }

  /** Creates a T from the given [[ScriptToken]]s */
  def fromAsm(asm: Seq[ScriptToken]): T

  def fromBytes(bytes: scodec.bits.ByteVector): T = {
    val cpmct = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val (_, noCmpctUInt) = bytes.splitAt(cpmct.bytes.size)
    val asm = ScriptParser.fromBytes(noCmpctUInt)
    fromAsm(asm)
  }

  /**
   * Scripts are serialized with a [[org.bitcoins.core.protocol.CompactSizeUInt]] at the beginning
   * to indicate how long the Script is. This construct assumes the [[CompactSizeUInt]]
   * is NOT passed into the constructor. Only the actual Script program bytes.
   * @param bytes
   * @return
   */
  def fromAsmBytes(bytes: scodec.bits.ByteVector): T = {
    val cmpct = CompactSizeUInt.calc(bytes)
    val fullBytes = cmpct.bytes ++ bytes
    fromBytes(fullBytes)
  }

  /**
   * Scripts are serialized with a [[org.bitcoins.core.protocol.CompactSizeUInt]] at the beginning
   * to indicate how long the [[ScriptSignature]] is. This construct assumes the [[CompactSizeUInt]]
   * is NOT passed into the constructor. Only the actual Script program hex is.
   * @param hex
   * @return
   */
  def fromAsmHex(hex: String): T = {
    fromAsmBytes(BitcoinSUtil.decodeHex(hex))
  }
}
