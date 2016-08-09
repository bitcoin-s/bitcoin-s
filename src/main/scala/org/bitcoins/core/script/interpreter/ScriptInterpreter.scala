
package org.bitcoins.core.script.interpreter

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{EmptyTransactionOutPoint, Transaction}
import org.bitcoins.core.script._
import org.bitcoins.core.script.arithmetic._
import org.bitcoins.core.script.bitwise._
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.control._
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.flag._
import org.bitcoins.core.script.locktime.{LockTimeInterpreter, OP_CHECKLOCKTIMEVERIFY, OP_CHECKSEQUENCEVERIFY}
import org.bitcoins.core.script.reserved._
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.splice._
import org.bitcoins.core.script.stack._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}

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
  def run(program : PreExecutionScriptProgram) : ScriptResult = {

    val scriptSig = program.txSignatureComponent.scriptSignature
    val scriptPubKey = program.txSignatureComponent.scriptPubKey
    val programBeingExecuted = ScriptProgram.toExecutionInProgress(program)
    val executedProgram : ExecutedScriptProgram = if (ScriptFlagUtil.requirePushOnly(program.flags)
      && !BitcoinScriptUtil.isPushOnly(program.script)) {
      logger.error("We can only have push operations inside of the script sig when the SIGPUSHONLY flag is set")
      ScriptProgram(programBeingExecuted,ScriptErrorSigPushOnly)
    } else if (scriptSig.isInstanceOf[P2SHScriptSignature] && ScriptFlagUtil.p2shEnabled(program.flags) &&
      !BitcoinScriptUtil.isPushOnly(scriptSig.asm)) {
      logger.error("P2SH scriptSigs are required to be push only by definition - see BIP16")
      ScriptProgram(programBeingExecuted,ScriptErrorSigPushOnly)
    } else {
      val scriptSigExecutedProgram = loop(program,0)
      scriptPubKey match {
        case p2shScriptPubKey : P2SHScriptPubKey if (ScriptFlagUtil.p2shEnabled(program.flags)) =>
          executeP2shScript(scriptSigExecutedProgram, programBeingExecuted, p2shScriptPubKey)
        case _ : MultiSignatureScriptPubKey | _ : P2SHScriptPubKey | _ : P2PKHScriptPubKey |
          _ : P2PKScriptPubKey | _ : NonStandardScriptPubKey | EmptyScriptPubKey =>
          logger.info("Stack state after scriptSig execution: " + scriptSigExecutedProgram.stack)
          if (!scriptSigExecutedProgram.error.isDefined) {
            logger.debug("We do not check a redeemScript against a non p2sh scriptSig")
            //now run the scriptPubKey script through the interpreter with the scriptSig as the stack arguments
            val scriptPubKeyProgram = ScriptProgram(scriptSigExecutedProgram.txSignatureComponent,
              scriptSigExecutedProgram.stack,scriptSigExecutedProgram.txSignatureComponent.scriptPubKey.asm)
            require(scriptPubKeyProgram.script == scriptSigExecutedProgram.txSignatureComponent.scriptPubKey.asm)
            val scriptPubKeyExecutedProgram : ExecutedScriptProgram = loop(scriptPubKeyProgram,0)

            logger.info("Stack state after scriptPubKey execution: " + scriptPubKeyExecutedProgram.stack)

            //if the program is valid, return if the stack top is true
            //else the program is false since something illegal happened during script evaluation
            scriptPubKeyExecutedProgram
          } else scriptSigExecutedProgram
      }
    }
    logger.debug("Executed Script Program: " + executedProgram)
    if (executedProgram.error.isDefined) executedProgram.error.get
    else if (executedProgram.stackTopIsTrue && executedProgram.flags.contains(ScriptVerifyCleanStack)) {
      //require that the stack after execution has exactly one element on it
      executedProgram.stack.size == 1 match {
        case true => ScriptOk
        case false => ScriptErrorCleanStack
      }
    } else if (executedProgram.stackTopIsTrue) ScriptOk
    else ScriptErrorEvalFalse
  }


  /**
    * P2SH scripts are unique in their evaluation, first the scriptSignature must be added to the stack, next the
    * p2sh scriptPubKey must be run to make sure the serialized redeem script hashes to the value found in the p2sh
    * scriptPubKey, then finally the serialized redeemScript is decoded and run with the arguments in the p2sh script signature
    * a p2sh script returns true if both of those intermediate steps evaluate to true
    * @param scriptSigExecutedProgram the program with the script signature pushed onto the stack
    * @param originalProgram the original program, used for setting errors & checking that the original script signature contains push only tokens
    * @param p2shScriptPubKey the p2sh scriptPubKey that contains the value the redeemScript must hash to
    * @return the executed program
    */
  private def executeP2shScript(scriptSigExecutedProgram : ExecutedScriptProgram, originalProgram : ScriptProgram, p2shScriptPubKey : P2SHScriptPubKey) : ExecutedScriptProgram = {
    val originalScriptSigAsm : Seq[ScriptToken] = scriptSigExecutedProgram.txSignatureComponent.scriptSignature.asm
    //need to check if the scriptSig is push only as required by bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L1268
    if (!BitcoinScriptUtil.isPushOnly(originalScriptSigAsm)) {
      ScriptProgram(originalProgram,ScriptErrorSigPushOnly)
    } else if (scriptSigExecutedProgram.error.isDefined) {
      scriptSigExecutedProgram
    } else {
      val hashCheckProgram = ScriptProgram(originalProgram, scriptSigExecutedProgram.stack, p2shScriptPubKey.asm)
      val hashesMatchProgram = loop(hashCheckProgram,0)
      hashesMatchProgram.stackTopIsTrue match {
        case true =>
          logger.info("Hashes matched between the p2shScriptSignature & the p2shScriptPubKey")
          //we need to run the deserialized redeemScript & the scriptSignature without the serialized redeemScript
          val stack = scriptSigExecutedProgram.stack

          logger.debug("P2sh stack: " + stack)
          val redeemScript = ScriptPubKey(stack.head.bytes)
          val p2shRedeemScriptProgram = ScriptProgram(scriptSigExecutedProgram.txSignatureComponent,stack.tail,
            redeemScript.asm)
          if (ScriptFlagUtil.requirePushOnly(p2shRedeemScriptProgram.flags) && !BitcoinScriptUtil.isPushOnly(redeemScript.asm)) {
            logger.error("p2sh redeem script must be push only operations whe SIGPUSHONLY flag is set")
            ScriptProgram(p2shRedeemScriptProgram,ScriptErrorSigPushOnly)
          } else loop(p2shRedeemScriptProgram,0)
        case false =>
          logger.warn("P2SH scriptPubKey hash did not match the hash for the serialized redeemScript")
          hashesMatchProgram
      }
    }
  }

  /**
    * The execution loop for a script
    * @param program the program whose script needs to be evaluated
    * @return program the final state of the program after being evaluated by the interpreter
    */
  @tailrec
  private def loop(program : ScriptProgram, opCount: Int) : ExecutedScriptProgram = {
    logger.debug("Stack: " + program.stack)
    logger.debug("Script: " + program.script)

    if (opCount > maxScriptOps && !program.isInstanceOf[ExecutedScriptProgram]) {
      logger.error("We have reached the maximum amount of script operations allowed")
      logger.error("Here are the remaining operations in the script: " + program.script)
      loop(ScriptProgram(program,ScriptErrorOpCount),opCount)
    } else if (program.script.flatMap(_.bytes).size > 10000 && !program.isInstanceOf[ExecutedScriptProgram]) {
      logger.error("We cannot run a script that is larger than 10,000 bytes")
      program match {
        case p : PreExecutionScriptProgram =>
          loop(ScriptProgram(ScriptProgram.toExecutionInProgress(p), ScriptErrorScriptSize),opCount)
        case _ : ExecutionInProgressScriptProgram | _ : ExecutedScriptProgram =>
          loop(ScriptProgram(program, ScriptErrorScriptSize),opCount)
      }
    } else {
      program match {
        case p : PreExecutionScriptProgram => loop(ScriptProgram.toExecutionInProgress(p,Some(p.stack)),opCount)
        case p : ExecutedScriptProgram =>
          //reset opCount variable to zero since we may need to count the ops
          //in the scriptPubKey - we don't want the op count of the scriptSig
          //to count towards the scriptPubKey op count
          logger.info("Final op count: " + opCount)
          p
        case p : ExecutionInProgressScriptProgram =>
          //increment the op count
/*          if (p.script.headOption.isDefined &&
            BitcoinScriptUtil.countsTowardsScriptOpLimit(p.script.head)) opCount = opCount + 1*/
          p.script match {
            //if at any time we see that the program is not valid
            //cease script execution
            case _ if !p.script.intersect(Seq(OP_VERIF, OP_VERNOTIF)).isEmpty =>
              logger.error("Script is invalid even when a OP_VERIF or OP_VERNOTIF occurs in an unexecuted OP_IF branch")
              loop(ScriptProgram(p, ScriptErrorBadOpCode),opCount)
            //disabled splice operation
            case _ if !p.script.intersect(Seq(OP_CAT, OP_SUBSTR, OP_LEFT, OP_RIGHT)).isEmpty =>
              logger.error("Script is invalid because it contains a disabled splice operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode),opCount)
            //disabled bitwise operations
            case _ if !p.script.intersect(Seq(OP_INVERT, OP_AND, OP_OR, OP_XOR)).isEmpty =>
              logger.error("Script is invalid because it contains a disabled bitwise operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode),opCount)
            //disabled arithmetic operations
            case _ if !p.script.intersect(Seq(OP_MUL, OP_2MUL, OP_DIV, OP_2DIV, OP_MOD, OP_LSHIFT, OP_RSHIFT)).isEmpty =>
              logger.error("Script is invalid because it contains a disabled arithmetic operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode),opCount)
            //program cannot contain a push operation > 520 bytes
            case _ if (p.script.exists(token => token.bytes.size > 520)) =>
              logger.error("We have a script constant that is larger than 520 bytes, this is illegal: " + p.script)
              loop(ScriptProgram(p, ScriptErrorPushSize),opCount)
            //program stack size cannot be greater than 1000 elements
            case _ if ((p.stack.size + p.altStack.size) > 1000) =>
              logger.error("We cannot have a stack + alt stack size larger than 1000 elements")
              loop(ScriptProgram(p, ScriptErrorStackSize),opCount)

            //stack operations
            case OP_DUP :: t => loop(opDup(p),calcOpCount(opCount,OP_DUP))
            case OP_DEPTH :: t => loop(opDepth(p),calcOpCount(opCount,OP_DEPTH))
            case OP_TOALTSTACK :: t => loop(opToAltStack(p),calcOpCount(opCount,OP_TOALTSTACK))
            case OP_FROMALTSTACK :: t => loop(opFromAltStack(p),calcOpCount(opCount,OP_FROMALTSTACK))
            case OP_DROP :: t => loop(opDrop(p),calcOpCount(opCount,OP_DROP))
            case OP_IFDUP :: t => loop(opIfDup(p),calcOpCount(opCount,OP_IFDUP))
            case OP_NIP :: t => loop(opNip(p),calcOpCount(opCount,OP_NIP))
            case OP_OVER :: t => loop(opOver(p),calcOpCount(opCount,OP_OVER))
            case OP_PICK :: t => loop(opPick(p),calcOpCount(opCount,OP_PICK))
            case OP_ROLL :: t => loop(opRoll(p),calcOpCount(opCount,OP_ROLL))
            case OP_ROT :: t => loop(opRot(p),calcOpCount(opCount,OP_ROT))
            case OP_2ROT :: t => loop(op2Rot(p),calcOpCount(opCount,OP_2ROT))
            case OP_2DROP :: t => loop(op2Drop(p),calcOpCount(opCount,OP_2DROP))
            case OP_SWAP :: t => loop(opSwap(p),calcOpCount(opCount,OP_SWAP))
            case OP_TUCK :: t => loop(opTuck(p),calcOpCount(opCount,OP_TUCK))
            case OP_2DUP :: t => loop(op2Dup(p),calcOpCount(opCount,OP_2DUP))
            case OP_3DUP :: t => loop(op3Dup(p),calcOpCount(opCount,OP_3DUP))
            case OP_2OVER :: t => loop(op2Over(p),calcOpCount(opCount,OP_2OVER))
            case OP_2SWAP :: t => loop(op2Swap(p),calcOpCount(opCount,OP_2SWAP))

            //arithmetic operations
            case OP_ADD :: t => loop(opAdd(p),calcOpCount(opCount,OP_ADD))
            case OP_1ADD :: t => loop(op1Add(p),calcOpCount(opCount,OP_1ADD))
            case OP_1SUB :: t => loop(op1Sub(p),calcOpCount(opCount,OP_1SUB))
            case OP_SUB :: t => loop(opSub(p),calcOpCount(opCount,OP_SUB))
            case OP_ABS :: t => loop(opAbs(p),calcOpCount(opCount,OP_ABS))
            case OP_NEGATE :: t => loop(opNegate(p),calcOpCount(opCount,OP_NEGATE))
            case OP_NOT :: t => loop(opNot(p),calcOpCount(opCount,OP_NOT))
            case OP_0NOTEQUAL :: t => loop(op0NotEqual(p),calcOpCount(opCount,OP_0NOTEQUAL))
            case OP_BOOLAND :: t => loop(opBoolAnd(p),calcOpCount(opCount,OP_BOOLAND))
            case OP_BOOLOR :: t => loop(opBoolOr(p),calcOpCount(opCount,OP_BOOLOR))
            case OP_NUMEQUAL :: t => loop(opNumEqual(p),calcOpCount(opCount,OP_NUMEQUAL))
            case OP_NUMEQUALVERIFY :: t => loop(opNumEqualVerify(p),calcOpCount(opCount,OP_NUMEQUALVERIFY))
            case OP_NUMNOTEQUAL :: t => loop(opNumNotEqual(p),calcOpCount(opCount,OP_NUMNOTEQUAL))
            case OP_LESSTHAN :: t => loop(opLessThan(p),calcOpCount(opCount,OP_LESSTHAN))
            case OP_GREATERTHAN :: t => loop(opGreaterThan(p),calcOpCount(opCount,OP_GREATERTHAN))
            case OP_LESSTHANOREQUAL :: t => loop(opLessThanOrEqual(p),calcOpCount(opCount,OP_LESSTHANOREQUAL))
            case OP_GREATERTHANOREQUAL :: t => loop(opGreaterThanOrEqual(p),calcOpCount(opCount,OP_GREATERTHANOREQUAL))
            case OP_MIN :: t => loop(opMin(p),calcOpCount(opCount,OP_MIN))
            case OP_MAX :: t => loop(opMax(p),calcOpCount(opCount,OP_MAX))
            case OP_WITHIN :: t => loop(opWithin(p),calcOpCount(opCount,OP_WITHIN))

            //bitwise operations
            case OP_EQUAL :: t => loop(opEqual(p),calcOpCount(opCount,OP_EQUAL))

            case OP_EQUALVERIFY :: t => loop(opEqualVerify(p),calcOpCount(opCount,OP_EQUALVERIFY))

            case OP_0 :: t => loop(ScriptProgram(p, ScriptNumber.zero :: p.stack, t),calcOpCount(opCount,OP_0))
            case (scriptNumberOp : ScriptNumberOperation) :: t =>
              loop(ScriptProgram(p, ScriptNumber(scriptNumberOp.underlying) :: p.stack, t),calcOpCount(opCount,scriptNumberOp))
            case (bytesToPushOntoStack: BytesToPushOntoStack) :: t =>
              loop(pushScriptNumberBytesToStack(p),calcOpCount(opCount,bytesToPushOntoStack))
            case (scriptNumber: ScriptNumber) :: t =>
              loop(ScriptProgram(p, scriptNumber :: p.stack, t),calcOpCount(opCount,scriptNumber))
            case OP_PUSHDATA1 :: t => loop(opPushData1(p),calcOpCount(opCount,OP_PUSHDATA1))
            case OP_PUSHDATA2 :: t => loop(opPushData2(p),calcOpCount(opCount,OP_PUSHDATA2))
            case OP_PUSHDATA4 :: t => loop(opPushData4(p),calcOpCount(opCount,OP_PUSHDATA4))

            case (x : ScriptConstant) :: t => loop(ScriptProgram(p, x :: p.stack, t),calcOpCount(opCount,x))

            //control operations
            case OP_IF :: t => loop(opIf(p),calcOpCount(opCount,OP_IF))
            case OP_NOTIF :: t => loop(opNotIf(p),calcOpCount(opCount,OP_NOTIF))
            case OP_ELSE :: t => loop(opElse(p),calcOpCount(opCount,OP_ELSE))
            case OP_ENDIF :: t => loop(opEndIf(p),calcOpCount(opCount,OP_ENDIF))
            case OP_RETURN :: t => loop(opReturn(p),calcOpCount(opCount,OP_RETURN))

            case OP_VERIFY :: t => loop(opVerify(p),calcOpCount(opCount,OP_VERIFY))

            //crypto operations
            case OP_HASH160 :: t => loop(opHash160(p),calcOpCount(opCount,OP_HASH160))
            case OP_CHECKSIG :: t => loop(opCheckSig(p),calcOpCount(opCount,OP_CHECKSIG))
            case OP_CHECKSIGVERIFY :: t => loop(opCheckSigVerify(p),calcOpCount(opCount,OP_CHECKSIGVERIFY))
            case OP_SHA1 :: t => loop(opSha1(p),calcOpCount(opCount,OP_SHA1))
            case OP_RIPEMD160 :: t => loop(opRipeMd160(p),calcOpCount(opCount,OP_RIPEMD160))
            case OP_SHA256 :: t => loop(opSha256(p),calcOpCount(opCount,OP_SHA256))
            case OP_HASH256 :: t => loop(opHash256(p),calcOpCount(opCount,OP_HASH256))
            case OP_CODESEPARATOR :: t => loop(opCodeSeparator(p),calcOpCount(opCount,OP_CODESEPARATOR))
            case OP_CHECKMULTISIG :: t =>
              opCheckMultiSig(p) match {
                case newProgram : ExecutedScriptProgram =>
                  //script was marked invalid for other reasons, don't need to update the opcount
                  loop(newProgram,opCount)
                case newProgram @ (_ : ExecutionInProgressScriptProgram | _ : PreExecutionScriptProgram) =>
                  val newOpCount = calcOpCount(opCount,OP_CHECKMULTISIG) + BitcoinScriptUtil.numPossibleSignaturesOnStack(program).toInt
                  loop(newProgram,newOpCount)
              }

            case OP_CHECKMULTISIGVERIFY :: t =>
              opCheckMultiSigVerify(p) match {
                case newProgram : ExecutedScriptProgram =>
                  //script was marked invalid for other reasons, don't need to update the opcount
                  loop(newProgram,opCount)
                case newProgram @ (_ : ExecutionInProgressScriptProgram | _ : PreExecutionScriptProgram) =>
                  val newOpCount = calcOpCount(opCount,OP_CHECKMULTISIGVERIFY) + BitcoinScriptUtil.numPossibleSignaturesOnStack(program).toInt
                  loop(newProgram,newOpCount)
              }
            //reserved operations
            case OP_NOP :: t =>
              //script discourage upgradeable flag does not apply to a OP_NOP
              loop(ScriptProgram(p, p.stack, t),calcOpCount(opCount,OP_NOP))

            //if we see an OP_NOP and the DISCOURAGE_UPGRADABLE_OP_NOPS flag is set we must fail our program
            case (nop: NOP) :: t if ScriptFlagUtil.discourageUpgradableNOPs(p.flags) =>
              logger.error("We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
              loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),calcOpCount(opCount,nop))
            case (nop: NOP) :: t => loop(ScriptProgram(p, p.stack, t),calcOpCount(opCount,nop))
            case OP_RESERVED :: t =>
              logger.error("OP_RESERVED automatically marks transaction invalid")
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,OP_RESERVED))
            case OP_VER :: t =>
              logger.error("Transaction is invalid when executing OP_VER")
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,OP_VER))
            case OP_RESERVED1 :: t =>
              logger.error("Transaction is invalid when executing OP_RESERVED1")
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,OP_RESERVED1))
            case OP_RESERVED2 :: t =>
              logger.error("Transaction is invalid when executing OP_RESERVED2")
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,OP_RESERVED2))

            case (reservedOperation : ReservedOperation) :: t =>
              logger.error("Undefined operation found which automatically fails the script: " + reservedOperation)
              loop(ScriptProgram(p,ScriptErrorBadOpCode),calcOpCount(opCount,reservedOperation))
            //splice operations
            case OP_SIZE :: t => loop(opSize(p),calcOpCount(opCount,OP_SIZE))

            //locktime operations
            case OP_CHECKLOCKTIMEVERIFY :: t =>
              //check if CLTV is enforced yet
              if (ScriptFlagUtil.checkLockTimeVerifyEnabled(p.flags)) loop(opCheckLockTimeVerify(p),calcOpCount(opCount,OP_CHECKLOCKTIMEVERIFY))
              //if not, check to see if we should discourage p
              else if (ScriptFlagUtil.discourageUpgradableNOPs(p.flags)) {
                logger.error("We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
                loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),calcOpCount(opCount,OP_CHECKLOCKTIMEVERIFY))
              }
              //in this case, just reat OP_CLTV just like a NOP and remove it from the stack
              else loop(ScriptProgram(p, p.script.tail, ScriptProgram.Script),calcOpCount(opCount,OP_CHECKLOCKTIMEVERIFY))
            case OP_CHECKSEQUENCEVERIFY :: t =>
              //check if CLTV is enforced yet
              if (ScriptFlagUtil.checkSequenceVerifyEnabled(p.flags)) loop(opCheckSequenceVerify(p),calcOpCount(opCount,OP_CHECKSEQUENCEVERIFY))
              //if not, check to see if we should discourage p
              else if (ScriptFlagUtil.discourageUpgradableNOPs(p.flags)) {
                logger.error("We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
                loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),calcOpCount(opCount,OP_CHECKSEQUENCEVERIFY))
              }
              //in this case, just read OP_CSV just like a NOP and remove it from the stack
              else loop(ScriptProgram(p, p.script.tail, ScriptProgram.Script),calcOpCount(opCount,OP_CHECKSEQUENCEVERIFY))
            //no more script operations to run, return whether the program is valid and the final state of the program
            case Nil => loop(ScriptProgram.toExecutedProgram(p),opCount)
            case h :: t => throw new RuntimeException(h + " was unmatched")
          }
      }
    }
  }


  /**
    * Checks the validity of a transaction in accordance to bitcoin core's CheckTransaction function
    * https://github.com/bitcoin/bitcoin/blob/f7a21dae5dbf71d5bc00485215e84e6f2b309d0a/src/main.cpp#L939
 *
    * @param transaction
    * @return
    */
  def checkTransaction(transaction : Transaction) : Boolean = {
    val inputOutputsNotZero = !(transaction.inputs.isEmpty || transaction.outputs.isEmpty)
    //TODO: replace 1000000 with a value that represents the max block size
    val txNotLargerThanBlock = transaction.bytes.size < Consensus.maxBlockSize
    val outputsSpendValidAmountsOfMoney = !transaction.outputs.exists(o =>
      o.value < CurrencyUnits.zero || o.value > Consensus.maxMoney)

    val outputValues = transaction.outputs.map(_.value)
    val totalSpentByOutputs : CurrencyUnit = outputValues.fold(CurrencyUnits.zero)(_ + _)
    val allOutputsValidMoneyRange = validMoneyRange(totalSpentByOutputs)
    val prevOutputTxIds = transaction.inputs.map(_.previousOutput.txId)
    val noDuplicateInputs = prevOutputTxIds.distinct.size == prevOutputTxIds.size

    val isValidScriptSigForCoinbaseTx = transaction.isCoinbase match {
      case true => transaction.inputs.head.scriptSignature.size >= 2 &&
        transaction.inputs.head.scriptSignature.size <= 100
      case false =>
        //since this is not a coinbase tx we cannot have any empty previous outs inside of inputs
        !transaction.inputs.exists(_.previousOutput == EmptyTransactionOutPoint)
    }
    inputOutputsNotZero && txNotLargerThanBlock && outputsSpendValidAmountsOfMoney && noDuplicateInputs &&
      allOutputsValidMoneyRange && noDuplicateInputs && isValidScriptSigForCoinbaseTx
  }


  /**
    * Determines if the given currency unit is within the valid range for the system
 *
    * @param currencyUnit
    * @return
    */
  def validMoneyRange(currencyUnit : CurrencyUnit) : Boolean = {
    currencyUnit >= CurrencyUnits.zero && currencyUnit <= Consensus.maxMoney
  }

  /**
    * Calculates the new op count after the execution of the given [[ScriptToken]]
    * @param oldOpCount
    * @param token
    * @return
    */
  private def calcOpCount(oldOpCount: Int, token: ScriptToken):Int = BitcoinScriptUtil.countsTowardsScriptOpLimit(token) match {
    case true => oldOpCount + 1
    case false => oldOpCount
  }

}

object ScriptInterpreter extends ScriptInterpreter