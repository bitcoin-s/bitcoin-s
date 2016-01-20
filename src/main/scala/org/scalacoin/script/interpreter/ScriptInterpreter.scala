package org.scalacoin.script.interpreter

import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.script.bitwise.{OP_EQUAL, BitwiseInterpreter, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.control.ControlOperationsInterpreter
import org.scalacoin.script.crypto.{OP_CHECKSIG, OP_HASH160, CryptoInterpreter}
import org.scalacoin.script.stack.{OP_DEPTH, StackInterpreter, OP_DUP}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/6/16.
 */
trait ScriptInterpreter extends CryptoInterpreter with StackInterpreter with ControlOperationsInterpreter
  with BitwiseInterpreter {

  private def logger = LoggerFactory.getLogger(this.getClass().toString)
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
      val (stack,script) = (scripts._1, scripts._2)
      script match {
        //stack operations
        case OP_DUP :: t => loop(opDup(stack,script))
        case OP_DEPTH :: t => loop(opDepth(stack,script))

        //bitwise operations
        case OP_EQUAL :: t => {
          val (newStack,newScript) = equal(stack, script)
          logger.debug("New stack: " + newStack)
          logger.debug("New script: " + newScript)
          if (newStack.head == ScriptTrue && newScript.size == 0) true
          else if (newStack.head == ScriptFalse && newScript.size == 0) false
          else loop(newStack,newScript)
        }
        case OP_EQUALVERIFY :: t => equalVerify(stack,script)

        //script constants
        //TODO: Implement these
        case ScriptConstantImpl(x) :: t if x == "1" => throw new RuntimeException("Not implemented yet")
        case ScriptConstantImpl(x) :: t if x == "0" => throw new RuntimeException("Not implemented yet")
        case (scriptNumber : ScriptNumber)::t => loop(scriptNumber :: stack, t)

        //TODO: is this right? I need to just push a constant on the input stack???
        case ScriptConstantImpl(x) :: t => loop((ScriptConstantImpl(x) :: stack, t))

        //crypto operations
        case OP_HASH160 :: t => loop(hash160(stack,script))
        case OP_CHECKSIG :: t => checkSig(stack,script,fullScript)
        //no more script operations to run, if stack top is true or '1' then it is a valid script
        case Nil => stack.head == OP_1 || stack.head == ScriptTrue
        case h :: t => throw new RuntimeException(h + " was unmatched")
      }
    }

    loop((List(),fullScript))
  }

  def run(inputScript : Seq[ScriptToken], outputScript : Seq[ScriptToken]) : Boolean = {
    run(inputScript.toList, outputScript.toList)
  }

  def run(scriptSignature : ScriptSignature, scriptPubKey : ScriptPubKey) : Boolean = {
    run(scriptSignature.asm, scriptPubKey.asm)
  }

}

object ScriptInterpreter extends ScriptInterpreter