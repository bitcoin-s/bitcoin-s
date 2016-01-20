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
      val (stack,outputScript) = (scripts._1, scripts._2)
      outputScript match {
        //stack operations
        case OP_DUP :: t => loop(opDup(stack,outputScript))
        case OP_DEPTH :: t => loop(opDepth(stack,outputScript))

        //bitwise operations
        case OP_EQUAL :: t => {
          val (newInputScript,newOutputScript) = equal(stack, outputScript)
          logger.debug("New input script: " + newInputScript)
          logger.debug("new output script: " + newOutputScript)
          if (newInputScript.head == ScriptTrue && newOutputScript.size == 0) true
          else if (newInputScript.head == ScriptFalse && newOutputScript.size == 0) false
          else loop(newInputScript,newOutputScript)
        }
        case OP_EQUALVERIFY :: t => equalVerify(stack,outputScript)

        //script constants
        //TODO: Implement these
        case ScriptConstantImpl(x) :: t if x == "1" => throw new RuntimeException("Not implemented yet")
        case ScriptConstantImpl(x) :: t if x == "0" => throw new RuntimeException("Not implemented yet")
        case OP_0 :: t => loop(OP_0 :: stack, t)
        //TODO: is this right? I need to just push a constant on the input stack???
        case ScriptConstantImpl(x) :: t => loop((ScriptConstantImpl(x) :: stack, t))

        //crypto operations
        case OP_HASH160 :: t => loop(hash160(stack,outputScript))
        case OP_CHECKSIG :: t => checkSig(stack,outputScript,fullScript)
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