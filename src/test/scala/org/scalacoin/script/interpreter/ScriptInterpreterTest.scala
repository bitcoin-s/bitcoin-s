package org.scalacoin.script.interpreter

import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.{ConstantImpl, ScriptOperation}
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160}
import org.scalacoin.script.stack.OP_DUP
import org.scalatest.{MustMatchers, FlatSpec}

/**
 * Created by chris on 1/6/16.
 */
class ScriptInterpreterTest extends FlatSpec with MustMatchers with ScriptInterpreter {



/*  "ScriptInterpreter" must "evaluate a valid script to true" in {
    //this is in asm format, not hex
    val inputScript =
      List("3044022016ffdbb7c57634903c5e018fcfc48d59f4e37dc4bc3bbc9ba4e6ee39150bca030220119c2241a931819bc1a75d3596e4029d803d1cd6de123bf8a1a1a2c3665e1fac01",
        "02af7dad03e682fcd0427b5c24140c220ac9d8abe286c15f8cf5bf77eed19c3652")
    //this is asm format, not hex
    val outputScript : List[ScriptOperation] =
      List(OP_DUP,OP_HASH160,ConstantImpl("e2e7c1ab3f807151e832dd1accb3d4f5d7d19b4b"),OP_EQUALVERIFY, OP_CHECKSIG)
    val result = run(inputScript, outputScript)
    result must be (true)
  }*/


}
