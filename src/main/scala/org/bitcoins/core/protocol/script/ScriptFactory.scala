package org.bitcoins.core.protocol.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util.Factory

/**
 * Created by chris on 12/9/16.
 */
trait ScriptFactory[T] extends Factory[T] {

  /** Builds a script from the given asm with the given constructor if the invariant holds true, else throws an error */
  def buildScript(asm: Seq[ScriptToken], constructor: Seq[Byte] => T,
                  invariant: Seq[ScriptToken] => Boolean, errorMsg: String): T = {
    if (invariant(asm)) {
      val asmBytes = asm.flatMap(_.bytes)
      val compactSizeUInt = CompactSizeUInt.calc(asmBytes)
      constructor(compactSizeUInt.bytes ++ asmBytes)
    } else throw new IllegalArgumentException(errorMsg)
  }

  /** Creates a T from the given [[ScriptToken]]s */
  def fromAsm(asm: Seq[ScriptToken]): T

  def fromBytes(bytes: Seq[Byte]): T = {
    val cpmct = CompactSizeUInt.parseCompactSizeUInt(bytes)
    val (_, noCmpctUInt) = bytes.splitAt(cpmct.bytes.size)
    val asm = ScriptParser.fromBytes(noCmpctUInt)
    fromAsm(asm)
  }

}
