package org.scalacoin.script.interpreter

import org.scalacoin.protocol.script.{ScriptSignature, ScriptPubKey}
import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.script.splice.{SpliceInterpreter, OP_SIZE}
import org.scalacoin.script.{ScriptProgramImpl, ScriptProgram}
import org.scalacoin.script.arithmetic._
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
  with BitwiseInterpreter with ConstantInterpreter with ArithmeticInterpreter with SpliceInterpreter {

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
        case OP_IFDUP :: t => loop(opIfDup(program))
        case OP_NIP :: t => loop(opNip(program))
        case OP_OVER :: t => loop(opOver(program))
        case OP_PICK :: t => loop(opPick(program))
        case OP_ROLL :: t => loop(opRoll(program))
        case OP_ROT :: t => loop(opRot(program))
        case OP_2ROT :: t => loop(op2Rot(program))
        case OP_2DROP :: t => loop(op2Drop(program))
        case OP_SWAP :: t => loop(opSwap(program))
        case OP_TUCK :: t => loop(opTuck(program))
        case OP_2DUP :: t => loop(op2Dup(program))
        case OP_3DUP :: t => loop(op3Dup(program))
        case OP_2OVER :: t => loop(op2Over(program))
        case OP_2SWAP :: t => loop(op2Swap(program))

        //arithmetic operations
        case OP_ADD :: t => loop(opAdd(program))
        case OP_1ADD :: t => loop(op1Add(program))
        case OP_1SUB :: t => loop(op1Sub(program))
        case OP_SUB :: t => loop(opSub(program))
        case OP_ABS :: t => loop(opAbs(program))

        //bitwise operations
        case OP_EQUAL :: t => {
          val newProgram = opEqual(program)
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
          if (scriptNumberOp == OP_0) loop(ScriptProgramImpl(OP_0 :: program.stack, t,program.transaction,program.altStack))
          else loop(ScriptProgramImpl(scriptNumberOp.scriptNumber :: program.stack, t,program.transaction,program.altStack))
        case (bytesToPushOntoStack : BytesToPushOntoStack) :: t => loop(pushScriptNumberBytesToStack(program))
        case (scriptNumber : ScriptNumber) :: t =>
          loop(ScriptProgramImpl(scriptNumber :: program.stack, t, program.transaction, program.altStack))
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

        //splice operations
        case OP_SIZE :: t => loop(opSize(program))
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