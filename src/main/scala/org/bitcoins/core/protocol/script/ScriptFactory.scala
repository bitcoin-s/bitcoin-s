package org.bitcoins.core.protocol.script

import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.script.constant.ScriptToken
import org.bitcoins.core.util.Factory

/**
  * Created by chris on 12/9/16.
  */
trait ScriptFactory[T] extends Factory[T] {

  /** Builds a script from the given asm with the given constructor if the invariant holds true, else throws an error */
  def buildScript(asm: Seq[ScriptToken], constructor: String => T, invariant: Seq[ScriptToken] => Boolean, errorMsg: String): T = {
    if (invariant(asm)) {
      val asmHex = asm.map(_.hex).mkString
      val compactSizeUInt = CompactSizeUInt.calculateCompactSizeUInt(asmHex)
      constructor(compactSizeUInt.hex + asmHex)
    } else throw new IllegalArgumentException(errorMsg)
  }

  /** Creates a T from the given [[ScriptToken]]s */
  def fromAsm(asm: Seq[ScriptToken]): T

}
