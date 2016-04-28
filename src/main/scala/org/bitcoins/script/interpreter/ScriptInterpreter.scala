package org.bitcoins.script.interpreter

import org.bitcoins.protocol.script._
import org.bitcoins.protocol.transaction.Transaction
import org.bitcoins.script.error._
import org.bitcoins.script.flag._
import org.bitcoins.script.locktime.{OP_CHECKLOCKTIMEVERIFY, LockTimeInterpreter}
import org.bitcoins.script.splice._
import org.bitcoins.script.{ExecutionInProgressScriptProgram, PreExecutionScriptProgram, ExecutedScriptProgram, ScriptProgram}
import org.bitcoins.script.arithmetic._
import org.bitcoins.script.bitwise._
import org.bitcoins.script.constant._
import org.bitcoins.script.control._
import org.bitcoins.script.crypto._
import org.bitcoins.script.reserved._
import org.bitcoins.script.stack._
import org.bitcoins.util.{BitcoinScriptUtil, BitcoinSLogger}
import org.slf4j.LoggerFactory

import scala.annotation.tailrec

/**
 * Created by chris on 1/6/16.
 */
trait ScriptInterpreter extends CryptoInterpreter with StackInterpreter with ControlOperationsInterpreter
  with BitwiseInterpreter with ConstantInterpreter with ArithmeticInterpreter with SpliceInterpreter
  with LockTimeInterpreter with BitcoinSLogger {


  /**
   * Currently bitcoin core limits the maximum number of non-push operations per script
   * to 201
   */
  private lazy val maxScriptOps = 201

  /**
   * Runs an entire script though our script programming language and
   * returns true or false depending on if the script was valid
 *
   * @param program the program to be interpreted
   * @return
   */
  def run(program : PreExecutionScriptProgram) : Boolean = {
    //TODO: Think about this more to change this from being mutable to immutable - perhaps
    //passing this as an implicit argument to our loop function
    /**
     * The current operation count that a script has performed
     * This is limited by bitcoin core to be 201 operations
     *
     */
    var opCount = 0
    /**
     *
     * @param program
     * @return boolean this boolean represents if the program hit any invalid states within the execution
     *         this does NOT indicate if the final value of the stack is true/false
     * @return program the final state of the program after being evaluated by the interpreter
     */
    @tailrec
    def loop(program : ScriptProgram) : ExecutedScriptProgram = {
      logger.debug("Stack: " + program.stack)
      logger.debug("Script: " + program.script)

      if (opCount > maxScriptOps && !program.isInstanceOf[ExecutedScriptProgram]) {
        logger.error("We have reached the maximum amount of script operations allowed")
        logger.error("Here are the remaining operations in the script: " + program.script)
        loop(ScriptProgram(program,ScriptErrorOpCount))
      } else if (program.script.flatMap(_.bytes).size > 10000 && !program.isInstanceOf[ExecutedScriptProgram]) {
        logger.error("We cannot run a script that is larger than 10,000 bytes")
        program match {
          case p : PreExecutionScriptProgram =>
            loop(ScriptProgram(ScriptProgram.toExecutionInProgress(p), ScriptErrorScriptSize))
          case _ : ExecutionInProgressScriptProgram | _ : ExecutedScriptProgram =>
            loop(ScriptProgram(program, ScriptErrorScriptSize))
        }
      } else {
        program match {
          case p : PreExecutionScriptProgram => loop(ScriptProgram.toExecutionInProgress(p,Some(p.stack)))
          case p : ExecutedScriptProgram =>
            //reset opCount variable to zero since we may need to count the ops
            //in the scriptPubKey - we don't want the op count of the scriptSig
            //to count towards the scriptPubKey op count
            logger.info("Final op count: " + opCount)
            opCount = 0
            p
          case p : ExecutionInProgressScriptProgram =>
            //increment the op count
            if (p.script.headOption.isDefined &&
              BitcoinScriptUtil.countsTowardsScriptOpLimit(p.script.head)) opCount = opCount + 1
            p.script match {
              //if at any time we see that the program is not valid
              //cease script execution
              case _ if !p.script.intersect(Seq(OP_VERIF, OP_VERNOTIF)).isEmpty =>
                logger.error("Script is invalid even when a OP_VERIF or OP_VERNOTIF occurs in an unexecuted OP_IF branch")
                loop(ScriptProgram(p, ScriptErrorDisabledOpCode))
              //disabled splice operation
              case _ if !p.script.intersect(Seq(OP_CAT, OP_SUBSTR, OP_LEFT, OP_RIGHT)).isEmpty =>
                logger.error("Script is invalid because it contains a disabled splice operation")
                loop(ScriptProgram(p, ScriptErrorDisabledOpCode))
              //disabled bitwise operations
              case _ if !p.script.intersect(Seq(OP_INVERT, OP_AND, OP_OR, OP_XOR)).isEmpty =>
                logger.error("Script is invalid because it contains a disabled bitwise operation")
                loop(ScriptProgram(p, ScriptErrorDisabledOpCode))
              //disabled arithmetic operations
              case _ if !p.script.intersect(Seq(OP_MUL, OP_2MUL, OP_DIV, OP_2DIV, OP_MOD, OP_LSHIFT, OP_RSHIFT)).isEmpty =>
                logger.error("Script is invalid because it contains a disabled arithmetic operation")
                loop(ScriptProgram(p, ScriptErrorDisabledOpCode))
              //program cannot contain a push operation > 520 bytes
              case _ if (p.script.exists(token => token.bytes.size > 520)) =>
                logger.error("We have a script constant that is larger than 520 bytes, this is illegal: " + p.script)
                loop(ScriptProgram(p, ScriptErrorPushSize))
              //program stack size cannot be greater than 1000 elements
              case _ if ((p.stack.size + p.altStack.size) > 1000) =>
                logger.error("We cannot have a stack + alt stack size larger than 1000 elements")
                loop(ScriptProgram(p, ScriptErrorStackSize))

              //stack operations
              case OP_DUP :: t => loop(opDup(p))
              case OP_DEPTH :: t => loop(opDepth(p))
              case OP_TOALTSTACK :: t => loop(opToAltStack(p))
              case OP_FROMALTSTACK :: t => loop(opFromAltStack(p))
              case OP_DROP :: t => loop(opDrop(p))
              case OP_IFDUP :: t => loop(opIfDup(p))
              case OP_NIP :: t => loop(opNip(p))
              case OP_OVER :: t => loop(opOver(p))
              case OP_PICK :: t => loop(opPick(p))
              case OP_ROLL :: t => loop(opRoll(p))
              case OP_ROT :: t => loop(opRot(p))
              case OP_2ROT :: t => loop(op2Rot(p))
              case OP_2DROP :: t => loop(op2Drop(p))
              case OP_SWAP :: t => loop(opSwap(p))
              case OP_TUCK :: t => loop(opTuck(p))
              case OP_2DUP :: t => loop(op2Dup(p))
              case OP_3DUP :: t => loop(op3Dup(p))
              case OP_2OVER :: t => loop(op2Over(p))
              case OP_2SWAP :: t => loop(op2Swap(p))

              //arithmetic operations
              case OP_ADD :: t => loop(opAdd(p))
              case OP_1ADD :: t => loop(op1Add(p))
              case OP_1SUB :: t => loop(op1Sub(p))
              case OP_SUB :: t => loop(opSub(p))
              case OP_ABS :: t => loop(opAbs(p))
              case OP_NEGATE :: t => loop(opNegate(p))
              case OP_NOT :: t => loop(opNot(p))
              case OP_0NOTEQUAL :: t => loop(op0NotEqual(p))
              case OP_BOOLAND :: t => loop(opBoolAnd(p))
              case OP_BOOLOR :: t => loop(opBoolOr(p))
              case OP_NUMEQUAL :: t => loop(opNumEqual(p))
              case OP_NUMEQUALVERIFY :: t => loop(opNumEqualVerify(p))
              case OP_NUMNOTEQUAL :: t => loop(opNumNotEqual(p))
              case OP_LESSTHAN :: t => loop(opLessThan(p))
              case OP_GREATERTHAN :: t => loop(opGreaterThan(p))
              case OP_LESSTHANOREQUAL :: t => loop(opLessThanOrEqual(p))
              case OP_GREATERTHANOREQUAL :: t => loop(opGreaterThanOrEqual(p))
              case OP_MIN :: t => loop(opMin(p))
              case OP_MAX :: t => loop(opMax(p))
              case OP_WITHIN :: t => loop(opWithin(p))

              //bitwise operations
              case OP_EQUAL :: t =>
                val newProgram = opEqual(p)
                loop(newProgram)

              case OP_EQUALVERIFY :: t => loop(opEqualVerify(p))

              case OP_0 :: t => loop(ScriptProgram(p, ScriptNumber.zero :: p.stack, t))
              case (scriptNumberOp : ScriptNumberOperation) :: t =>
                loop(ScriptProgram(p, ScriptNumber(scriptNumberOp.num) :: p.stack, t))
              case (bytesToPushOntoStack: BytesToPushOntoStack) :: t => loop(pushScriptNumberBytesToStack(p))
              case (scriptNumber: ScriptNumber) :: t =>
                loop(ScriptProgram(p, scriptNumber :: p.stack, t))
              case OP_PUSHDATA1 :: t => loop(opPushData1(p))
              case OP_PUSHDATA2 :: t => loop(opPushData2(p))
              case OP_PUSHDATA4 :: t => loop(opPushData4(p))

              case (x : ScriptConstant) :: t => loop(ScriptProgram(p, x :: p.stack, t))

              //control operations
              case OP_IF :: t => loop(opIf(p))
              case OP_NOTIF :: t => loop(opNotIf(p))
              case OP_ELSE :: t => loop(opElse(p))
              case OP_ENDIF :: t => loop(opEndIf(p))
              case OP_RETURN :: t => loop(opReturn(p))

              case OP_VERIFY :: t => loop(opVerify(p))

              //crypto operations
              case OP_HASH160 :: t => loop(opHash160(p))
              case OP_CHECKSIG :: t => loop(opCheckSig(p))
              case OP_SHA1 :: t => loop(opSha1(p))
              case OP_RIPEMD160 :: t => loop(opRipeMd160(p))
              case OP_SHA256 :: t => loop(opSha256(p))
              case OP_HASH256 :: t => loop(opHash256(p))
              case OP_CODESEPARATOR :: t => loop(opCodeSeparator(p))
              case OP_CHECKMULTISIG :: t =>
                opCheckMultiSig(p) match {
                  case newProgram : ExecutedScriptProgram =>
                    //script was marked invalid for other reasons, don't need to update the opcount
                    loop(newProgram)
                  case newProgram : ExecutionInProgressScriptProgram =>
                    opCount = opCount + BitcoinScriptUtil.numPossibleSignaturesOnStack(program).num.toInt
                    loop(newProgram)
                  case newProgram : PreExecutionScriptProgram =>
                    opCount = opCount + BitcoinScriptUtil.numPossibleSignaturesOnStack(program).num.toInt
                    loop(newProgram)
                }

              case OP_CHECKMULTISIGVERIFY :: t =>
                opCheckMultiSigVerify(p) match {
                  case newProgram : ExecutedScriptProgram =>
                    //script was marked invalid for other reasons, don't need to update the opcount
                    loop(newProgram)
                  case newProgram : ExecutionInProgressScriptProgram =>
                    opCount = opCount + BitcoinScriptUtil.numPossibleSignaturesOnStack(program).num.toInt
                    loop(newProgram)
                  case newProgram : PreExecutionScriptProgram =>
                    opCount = opCount + BitcoinScriptUtil.numPossibleSignaturesOnStack(program).num.toInt
                    loop(newProgram)
                }
              //reserved operations
              case OP_NOP :: t =>
                //script discourage upgradeable flag does not apply to a OP_NOP
                loop(ScriptProgram(p, p.stack, t))

              //if we see an OP_NOP and the DISCOURAGE_UPGRADABLE_OP_NOPS flag is set we must fail our program
              case (nop: NOP) :: t if ScriptFlagUtil.discourageUpgradableNOPs(p.flags) =>
                logger.error("We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
                loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs))
              case (nop: NOP) :: t => loop(ScriptProgram(p, p.stack, t))
              case OP_RESERVED :: t =>
                logger.error("OP_RESERVED automatically marks transaction invalid")
                loop(ScriptProgram(p,ScriptErrorDisabledOpCode))
              case OP_VER :: t =>
                logger.error("Transaction is invalid when executing OP_VER")
                loop(ScriptProgram(p,ScriptErrorDisabledOpCode))
              case OP_RESERVED1 :: t =>
                logger.error("Transaction is invalid when executing OP_RESERVED1")
                loop(ScriptProgram(p,ScriptErrorDisabledOpCode))
              case OP_RESERVED2 :: t =>
                logger.error("Transaction is invalid when executing OP_RESERVED2")
                loop(ScriptProgram(p,ScriptErrorDisabledOpCode))

              case (reservedOperation : ReservedOperation) :: t =>
                logger.error("Undefined operation found which automatically fails the script: " + reservedOperation)
                loop(ScriptProgram(p,ScriptErrorBadOpCode))
              //splice operations
              case OP_SIZE :: t => loop(opSize(p))

              //locktime operations
              case OP_CHECKLOCKTIMEVERIFY :: t =>
                //check if CLTV is enforced yet
                if (ScriptFlagUtil.checkLockTimeVerifyEnabled(p.flags)) loop(opCheckLockTimeVerify(p))
                //if not, check to see if we should discourage p
                else if (ScriptFlagUtil.discourageUpgradableNOPs(p.flags)) {
                  logger.error("We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
                  loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs))
                }
                //in this case, just reat OP_CLTV just like a NOP and remove it from the stack
                else loop(ScriptProgram(p, p.script.tail, ScriptProgram.Script))
              //no more script operations to run, return whether the program is valid and the final state of the program
              case Nil => loop(ScriptProgram.toExecutedProgram(p))
              case h :: t => throw new RuntimeException(h + " was unmatched")
            }
        }
      }
    }


    val executedProgram : ExecutedScriptProgram = program.txSignatureComponent.scriptSignature match {
      //if the P2SH script flag is not set, we evaluate a p2sh scriptSig just like any other scriptSig
      case scriptSig : P2SHScriptSignature if (program.flags.contains(ScriptVerifyP2SH)) =>
        if (!BitcoinScriptUtil.isPushOnly(scriptSig.asm)) {
          val executionInProgress = ScriptProgram.toExecutionInProgress(program)
          logger.error("P2SH script signatures must be push only")
          ScriptProgram(executionInProgress,ScriptErrorSigPushOnly)
        } else {
          //first run the serialized redeemScript && the p2shScriptPubKey to see if the hashes match
          val hashCheckProgram = ScriptProgram(program, Seq(scriptSig.asm.last), program.txSignatureComponent.scriptPubKey.asm)
          val hashesMatchProgram = loop(hashCheckProgram)

          hashesMatchProgram.stackTopIsTrue match {
            case true =>
              logger.info("Hashes matched between the p2shScriptSignature & the p2shScriptPubKey")
              //we need to run the deserialized redeemScript & the scriptSignature without the serialized redeemScript
              val stack = BitcoinScriptUtil.filterPushOps(scriptSig.scriptSignatureNoRedeemScript.asm.reverse)
              logger.debug("P2sh stack: " + stack)
              logger.debug("P2sh redeemScript: " + scriptSig.redeemScript.asm)
              val p2shRedeemScriptProgram = ScriptProgram(hashesMatchProgram.txSignatureComponent,stack, scriptSig.redeemScript.asm)
              loop(p2shRedeemScriptProgram)
            case false =>
              logger.warn("P2SH scriptPubKey hash did not match the hash for the serialized redeemScript")
              hashesMatchProgram
          }
        }
      case _ : P2PKHScriptSignature | _ : P2PKScriptSignature | _ : MultiSignatureScriptSignature |
           _ : NonStandardScriptSignature | _ : P2SHScriptSignature | EmptyScriptSignature =>

        val scriptSigProgram = ScriptProgram(program,Seq(),program.txSignatureComponent.scriptSignature.asm)
        val scriptSigExecutedProgram = loop(scriptSigProgram)
        logger.info("Stack state after scriptSig execution: " + scriptSigExecutedProgram.stack)
        logger.info("scriptSigExecutedProgram: " + scriptSigExecutedProgram.error)
        if (!scriptSigExecutedProgram.error.isDefined) {
          logger.debug("We do not check a redeemScript against a non p2sh scriptSig")
          //now run the scriptPubKey script through the interpreter with the scriptSig as the stack arguments
          val scriptPubKeyProgram = ScriptProgram(scriptSigExecutedProgram.txSignatureComponent,
            scriptSigExecutedProgram.stack,scriptSigExecutedProgram.txSignatureComponent.scriptPubKey.asm)
          require(scriptPubKeyProgram.script == scriptSigExecutedProgram.txSignatureComponent.scriptPubKey.asm)
          val scriptPubKeyExecutedProgram : ExecutedScriptProgram = loop(scriptPubKeyProgram)

          logger.info("Stack state after scriptPubKey execution: " + scriptPubKeyExecutedProgram.stack)

          //if the program is valid, return if the stack top is true
          //else the program is false since something illegal happened during script evaluation
          scriptPubKeyExecutedProgram.error.isDefined match {
            case true =>  scriptPubKeyExecutedProgram
            case false => scriptPubKeyExecutedProgram
          }

        } else scriptSigExecutedProgram

    }

    if (executedProgram.error.isDefined) false
    else if (executedProgram.stackTopIsTrue && executedProgram.flags.contains(ScriptVerifyCleanStack)) {
      //require that the stack after execution has exactly one element on it
      executedProgram.stack.size == 1
    } else executedProgram.stackTopIsTrue

  }

}

object ScriptInterpreter extends ScriptInterpreter