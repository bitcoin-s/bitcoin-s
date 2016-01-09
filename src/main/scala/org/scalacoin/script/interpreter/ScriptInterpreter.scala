package org.scalacoin.script.interpreter

import org.scalacoin.script.bitwise.{OP_EQUAL, BitwiseInterpreter, OP_EQUALVERIFY}
import org.scalacoin.script.constant.{ScriptConstantImpl, ScriptToken}
import org.scalacoin.script.control.ControlOperationsInterpreter
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160, CryptoInterpreter}
import org.scalacoin.script.stack.{StackInterpreter, OP_DUP}

import scala.annotation.tailrec

/**
 * Created by chris on 1/6/16.
 */
trait ScriptInterpreter extends CryptoInterpreter with StackInterpreter with ControlOperationsInterpreter
  with BitwiseInterpreter {


  /**
   * Runs an entire script though our script programming language and
   * returns true or false depending on if the script was valid
   * @param stack
   * @param script
   * @return
   */

  def run(inputScript : List[ScriptToken], outputScript : List[ScriptToken]) : Boolean = {
    val fullInputScript = inputScript
    val fullOutputScript = outputScript
    val fullScript = inputScript ++ fullOutputScript
    @tailrec
    def loop(scripts : (List[ScriptToken], List[ScriptToken])) : Boolean = {
      val (inputScript,outputScript) = (scripts._1, scripts._2)
      outputScript match {
        case OP_DUP :: t => loop(opDup(inputScript,outputScript))
        case OP_HASH160 :: t => loop(hash160(inputScript,outputScript))
        case OP_EQUAL :: t => loop(equal(inputScript, outputScript))
        //TODO: Implement these
        case ScriptConstantImpl(x) :: t if x == "1" => throw new RuntimeException("Not implemented yet")
        case ScriptConstantImpl(x) :: t if x == "0" => throw new RuntimeException("Not implemented yet")
        //TODO: is this right? I need to just push a constant on the input stack???
        case ScriptConstantImpl(x)  :: t => loop((ScriptConstantImpl(x) :: inputScript, t))
        //these cases result in our boolean result
        case OP_EQUALVERIFY :: t => equalVerify(inputScript,outputScript)
        case OP_CHECKSIG :: t => checkSig(inputScript,outputScript,fullScript)
      }
    }

    loop((inputScript,outputScript))
  }


}
