package org.scalacoin.script.interpreter

import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.arithmetic.{ArithmeticInterpreter, OP_ADD}
import org.scalacoin.script.bitwise.{OP_EQUAL, BitwiseInterpreter, OP_EQUALVERIFY}
import org.scalacoin.script.constant._
import org.scalacoin.script.control._
import org.scalacoin.script.crypto.{OP_SHA1, OP_CHECKSIG, OP_HASH160, CryptoInterpreter}
import org.scalacoin.script.reserved.NOP
import org.scalacoin.script.stack._
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/6/16.
 */
trait ScriptInterpreter extends CryptoInterpreter with StackInterpreter with ControlOperationsInterpreter
  with BitwiseInterpreter with ConstantInterpreter with ArithmeticInterpreter {

  private def logger = LoggerFactory.getLogger(this.getClass().toString)

  /**
   * Runs an entire script though our script programming language and
   * returns true or false depending on if the script was valid
   * @param inputScript
   * @param outputScript
   * @param transaction
   * @return
   */
  def run(inputScript : List[ScriptToken], outputScript : List[ScriptToken], transaction : Transaction) : Boolean = {
    val fullInputScript = inputScript
    val fullOutputScript = outputScript
    val fullScript = inputScript ++ fullOutputScript

    @tailrec
    def loop(program : ScriptProgram) : Boolean = {
      logger.debug("Stack: " + program.stack)
      logger.debug("Script: " + program.script)
      program.script match {
        //stack operations
        case OP_DUP :: t => loop(opDup(program))
        case OP_DEPTH :: t => loop(opDepth(program))
        case OP_TOALTSTACK :: t => loop(opToAltStack(program))
        case OP_FROMALTSTACK :: t => loop(opFromAltStack(program))
        case OP_DROP :: t => loop(opDrop(program))
        //arithmetic operations
        case OP_ADD :: t => loop(opAdd(program))

        //bitwise operations
        case OP_EQUAL :: t => {
          val newProgram = equal(program)
          if (newProgram.stack.head == ScriptTrue && newProgram.script.size == 0) true
          else if (newProgram.stack.head == ScriptFalse && newProgram.script.size == 0) false
          else loop(newProgram)
        }
        case OP_EQUALVERIFY :: t => equalVerify(program).valid
        //script constants
        //TODO: Implement these
        case ScriptConstantImpl(x) :: t if x == "1" => throw new RuntimeException("Not implemented yet")
        case ScriptConstantImpl(x) :: t if x == "0" => throw new RuntimeException("Not implemented yet")
        case (scriptNumberOp : ScriptNumberOperation) :: t =>
          loop(ScriptProgramImpl(scriptNumberOp.scriptNumber :: program.stack, t,program.transaction,program.altStack))
        case (scriptNumber : ScriptNumber) :: t => loop(pushScriptNumberBytesToStack(program))
        case OP_PUSHDATA1 :: t => loop(opPushData1(program))
        case OP_PUSHDATA2 :: t => loop(opPushData2(program))
        case OP_PUSHDATA4 :: t => loop(opPushData4(program))

        //TODO: is this right? I need to just push a constant on the input stack???
        case ScriptConstantImpl(x) :: t => loop(ScriptProgramImpl(ScriptConstantImpl(x) :: program.stack, t,
          program.transaction,program.altStack))

        //control operations
        case OP_IF :: t => loop(opIf(program))
        case OP_NOTIF :: t => loop(opNotIf(program))
        case OP_ELSE :: t => loop(opElse(program))
        case OP_ENDIF :: t => loop(opEndIf(program))
        case OP_RETURN :: t => opReturn(program)
        case OP_VERIFY :: t =>
          val newProgram = opVerify(program)
          if (newProgram.valid) loop(newProgram)
          else false

        //crypto operations
        case OP_HASH160 :: t => loop(hash160(program))
        case OP_CHECKSIG :: t => checkSig(program.stack,program.script,fullScript)
        case OP_SHA1 :: t => loop(opSha1(program))

        //reserved operations
        case (nop : NOP) :: t => loop(ScriptProgramImpl(program.stack,t,program.transaction,program.altStack))

        //no more script operations to run, True is represented by any representation of non-zero
        case Nil => program.stack.head != ScriptFalse
        case h :: t => throw new RuntimeException(h + " was unmatched")
      }
    }

    loop(ScriptProgramImpl(List(),fullScript,transaction, List()))
  }

  def run(inputScript : Seq[ScriptToken], outputScript : Seq[ScriptToken], transaction : Transaction) : Boolean = {
    run(inputScript.toList, outputScript.toList,transaction)
  }

  def run(scriptSignature : ScriptSignature, scriptPubKey : ScriptPubKey, transaction : Transaction) : Boolean = {
    run(scriptSignature.asm, scriptPubKey.asm,transaction)
  }

}

object ScriptInterpreter extends ScriptInterpreter