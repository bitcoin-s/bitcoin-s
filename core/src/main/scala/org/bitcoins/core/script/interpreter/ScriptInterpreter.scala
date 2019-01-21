package org.bitcoins.core.script.interpreter

import org.bitcoins.core.consensus.Consensus
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script._
import org.bitcoins.core.script.arithmetic._
import org.bitcoins.core.script.bitwise._
import org.bitcoins.core.script.constant.{ScriptToken, _}
import org.bitcoins.core.script.control._
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.flag._
import org.bitcoins.core.script.locktime.{
  LockTimeInterpreter,
  OP_CHECKLOCKTIMEVERIFY,
  OP_CHECKSEQUENCEVERIFY
}
import org.bitcoins.core.script.reserved._
import org.bitcoins.core.script.result._
import org.bitcoins.core.script.splice._
import org.bitcoins.core.script.stack._
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, BitcoinScriptUtil}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 1/6/16.
  */
sealed abstract class ScriptInterpreter {

  private def logger = BitcoinSLogger.logger

  /**
    * Currently bitcoin core limits the maximum number of non-push operations per script
    * to 201
    */
  private lazy val maxScriptOps = 201

  /** We cannot push an element larger than 520 bytes onto the stack */
  private lazy val maxPushSize = 520

  /**
    * Runs an entire script though our script programming language and
    * returns a [[org.bitcoins.core.script.result.ScriptResult ScriptResult]]
    * indicating if the script was valid, or if not what error it encountered
    */
  def run(program: PreExecutionScriptProgram): ScriptResult = {
    val scriptSig = program.txSignatureComponent.scriptSignature
    val scriptPubKey = program.txSignatureComponent.scriptPubKey
    val flags = program.flags
    val p2shEnabled = ScriptFlagUtil.p2shEnabled(flags)
    val segwitEnabled = ScriptFlagUtil.segWitEnabled(flags)
    val executedProgram: ExecutedScriptProgram =
      if (ScriptFlagUtil.requirePushOnly(flags)
          && !BitcoinScriptUtil.isPushOnly(program.script)) {
        logger.error(
          "We can only have push operations inside of the script sig when the SIGPUSHONLY flag is set")
        ScriptProgram(program, ScriptErrorSigPushOnly)
      } else if (scriptSig.isInstanceOf[P2SHScriptSignature] && p2shEnabled &&
                 !BitcoinScriptUtil.isPushOnly(scriptSig.asm)) {
        logger.error(
          "P2SH scriptSigs are required to be push only by definition - see BIP16, got: " + scriptSig.asm)
        ScriptProgram(program, ScriptErrorSigPushOnly)
      } else {
        val scriptSigExecutedProgram = loop(program, 0)
        logger.trace(s"scriptSigExecutedProgram $scriptSigExecutedProgram")
        val t = scriptSigExecutedProgram.txSignatureComponent
        val scriptPubKeyProgram = ExecutionInProgressScriptProgram(
          t,
          scriptSigExecutedProgram.stack,
          t.scriptPubKey.asm.toList,
          t.scriptPubKey.asm.toList,
          Nil,
          scriptSigExecutedProgram.flags,
          None)
        val scriptPubKeyExecutedProgram: ExecutedScriptProgram =
          loop(scriptPubKeyProgram, 0)
        logger.trace(
          s"scriptPubKeyExecutedProgram $scriptPubKeyExecutedProgram")
        if (scriptSigExecutedProgram.error.isDefined) {
          scriptSigExecutedProgram
        } else if (scriptPubKeyExecutedProgram.error.isDefined || scriptPubKeyExecutedProgram.stackTopIsFalse) {
          scriptPubKeyExecutedProgram
        } else {
          scriptPubKey match {
            case witness: WitnessScriptPubKey =>
              if (segwitEnabled)
                executeSegWitScript(scriptPubKeyExecutedProgram, witness).get
              else scriptPubKeyExecutedProgram
            case _: P2SHScriptPubKey =>
              if (p2shEnabled) executeP2shScript(scriptSigExecutedProgram)
              else scriptPubKeyExecutedProgram
            case _: P2PKHScriptPubKey | _: P2PKScriptPubKey |
                _: MultiSignatureScriptPubKey | _: CSVScriptPubKey |
                _: CLTVScriptPubKey | _: NonStandardScriptPubKey |
                _: WitnessCommitment | EmptyScriptPubKey =>
              scriptPubKeyExecutedProgram
          }
        }
      }
    logger.trace("Executed Script Program: " + executedProgram)
    if (executedProgram.error.isDefined) executedProgram.error.get
    else if (hasUnexpectedWitness(program)) {
      //note: the 'program' value we pass above is intentional, we need to check the original program
      //as the 'executedProgram' may have had the scriptPubKey value changed to the rebuilt ScriptPubKey of the witness program

      ScriptErrorWitnessUnexpected
    } else if (executedProgram.stackTopIsTrue && flags.contains(
                 ScriptVerifyCleanStack)) {
      //require that the stack after execution has exactly one element on it
      if (executedProgram.stack.size == 1) ScriptOk
      else ScriptErrorCleanStack
    } else if (executedProgram.stackTopIsTrue) ScriptOk
    else ScriptErrorEvalFalse
  }

  /**
    * Runs the given [[org.bitcoins.core.script.PreExecutionScriptProgram PreExecutionScriptProgram]] and
    * return if that script was valid or not
    */
  def runVerify(p: PreExecutionScriptProgram): Boolean = {
    ScriptInterpreter.run(p) == ScriptOk
  }

  /**
    * Every given [[org.bitcoins.core.script.PreExecutionScriptProgram PreExecutionScriptProgram]] and returns
    * it's [[org.bitcoins.core.script.result.ScriptResult ScriptResult]]
    */
  def runAll(programs: Seq[PreExecutionScriptProgram]): Seq[ScriptResult] = {
    programs.map(p => ScriptInterpreter.run(p))
  }

  /**
    * Runs all the given [[org.bitcoins.core.script.ScriptProgram ScriptProgram]] and return
    * if it is valid or not
    */
  def runAllVerify(programs: Seq[PreExecutionScriptProgram]): Boolean = {
    !programs.exists(p => ScriptInterpreter.run(p) != ScriptOk)
  }

  /**
    * P2SH scripts are unique in their evaluation, first the scriptSignature must be added to the stack, next the
    * p2sh scriptPubKey must be run to make sure the serialized redeem script hashes to the value found in the p2sh
    * scriptPubKey, then finally the serialized redeemScript is decoded and run with the arguments in the p2sh script signature
    * a p2sh script returns true if both of those intermediate steps evaluate to true
    *
    * @param scriptPubKeyExecutedProgram the program with the script signature pushed onto the stack
    * @return the executed program
    */
  private def executeP2shScript(
      scriptPubKeyExecutedProgram: ExecutedScriptProgram): ExecutedScriptProgram = {
    val flags = scriptPubKeyExecutedProgram.flags

    val segwitEnabled = ScriptFlagUtil.segWitEnabled(flags)

    /** Helper function to actually run a p2sh script */
    def run(
        p: ExecutedScriptProgram,
        s: ScriptPubKey): ExecutedScriptProgram = {

      val p2shRedeemScriptProgram = ExecutionInProgressScriptProgram(
        txSignatureComponent = p.txSignatureComponent,
        stack = p.stack.tail,
        script = s.asm.toList,
        originalScript = p.originalScript,
        altStack = Nil,
        flags = p.flags,
        lastCodeSeparator = None
      )

      /*ScriptProgram(p.txSignatureComponent, stack.tail,s.asm)*/
      if (ScriptFlagUtil.requirePushOnly(p2shRedeemScriptProgram.flags) && !BitcoinScriptUtil
            .isPushOnly(s.asm)) {
        logger.error(
          "p2sh redeem script must be push only operations whe SIGPUSHONLY flag is set")
        ScriptProgram(p2shRedeemScriptProgram, ScriptErrorSigPushOnly)
      } else loop(p2shRedeemScriptProgram, 0)
    }

    val scriptSig =
      scriptPubKeyExecutedProgram.txSignatureComponent.scriptSignature
    val scriptSigAsm: Seq[ScriptToken] = scriptSig.asm
    //need to check if the scriptSig is push only as required by bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1419
    if (!BitcoinScriptUtil.isPushOnly(scriptSigAsm)) {
      ScriptProgram(scriptPubKeyExecutedProgram, ScriptErrorSigPushOnly)
    } else if (scriptPubKeyExecutedProgram.error.isDefined) {
      scriptPubKeyExecutedProgram
    } else {
      scriptPubKeyExecutedProgram.stackTopIsTrue match {
        case true =>
          logger.debug(
            "Hashes matched between the p2shScriptSignature & the p2shScriptPubKey")

          //we need to run the deserialized redeemScript & the scriptSignature without the serialized redeemScript
          val stack = scriptPubKeyExecutedProgram.stack

          val redeemScriptBytes = stack.head.bytes

          val c = CompactSizeUInt.calculateCompactSizeUInt(redeemScriptBytes)

          val redeemScript = ScriptPubKey(c.bytes ++ redeemScriptBytes)

          redeemScript match {

            case p2wpkh: P2WPKHWitnessSPKV0 =>
              val wtxSigP2SH = scriptPubKeyExecutedProgram.txSignatureComponent
                .asInstanceOf[WitnessTxSigComponentP2SH]

              //for the p2sh(p2wpkh) case
              //https://github.com/bitcoin/bitcoin/blob/78dae8caccd82cfbfd76557f1fb7d7557c7b5edb/src/script/interpreter.cpp#L1437

              val pushOp = BitcoinScriptUtil.calculatePushOp(redeemScriptBytes)

              val expectedScriptBytes = BitcoinSUtil.toByteVector(pushOp) ++ redeemScriptBytes

              val isExpectedScriptBytes = scriptSig.asmBytes == expectedScriptBytes
              if (segwitEnabled &&
                  wtxSigP2SH.witness.stack.size == 2 &&
                  isExpectedScriptBytes) {
                executeSegWitScript(scriptPubKeyExecutedProgram, p2wpkh).get
              } else if (segwitEnabled) {
                ScriptProgram(oldProgram = scriptPubKeyExecutedProgram,
                              error = ScriptErrorWitnessMalleatedP2SH)
              } else {
                //segwit not enabled, treat as old spk
                run(scriptPubKeyExecutedProgram, p2wpkh)
              }

            case p2wsh: P2WSHWitnessSPKV0 =>
              val pushOp = BitcoinScriptUtil.calculatePushOp(redeemScriptBytes)

              val expectedScriptBytes = BitcoinSUtil.toByteVector(pushOp) ++ redeemScriptBytes

              val isExpectedScriptBytes = scriptSig.asmBytes == expectedScriptBytes

              if (segwitEnabled && isExpectedScriptBytes) {
                // The scriptSig must be _exactly_ a single push of the redeemScript. Otherwise we
                // reintroduce malleability.
                logger.debug(
                  "redeem script was witness script pubkey, segwit was enabled, scriptSig was single push of redeemScript")
                //TODO: remove .get here
                executeSegWitScript(scriptPubKeyExecutedProgram, p2wsh).get
              } else if (segwitEnabled && (scriptSig.asmBytes != expectedScriptBytes)) {
                logger.error(
                  "Segwit was enabled, but p2sh redeem script was malleated")
                logger.error("ScriptSig bytes: " + scriptSig.hex)
                logger.error(
                  "expected scriptsig bytes: " + expectedScriptBytes.toHex)
                ScriptProgram(scriptPubKeyExecutedProgram,
                              ScriptErrorWitnessMalleatedP2SH)
              } else {
                logger.warn(
                  "redeem script was witness script pubkey, segwit was NOT enabled")
                //treat the segwit scriptpubkey as any other redeem script
                run(scriptPubKeyExecutedProgram, p2wsh)
              }
            case s @ (_: P2SHScriptPubKey | _: P2PKHScriptPubKey |
                _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey |
                _: CLTVScriptPubKey | _: CSVScriptPubKey |
                _: NonStandardScriptPubKey | _: WitnessCommitment |
                _: UnassignedWitnessScriptPubKey | EmptyScriptPubKey) =>
              run(scriptPubKeyExecutedProgram, s)
          }
        case false =>
          logger.warn(
            "P2SH scriptPubKey hash did not match the hash for the serialized redeemScript")
          scriptPubKeyExecutedProgram
      }
    }

  }

  /**
    * Runs a segwit script through our interpreter, mimics this functionality in bitcoin core:
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1441-L1452]]
    * @param scriptPubKeyExecutedProgram the program with the
    *                                    [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]] executed
    */
  private def executeSegWitScript(
      scriptPubKeyExecutedProgram: ExecutedScriptProgram,
      witnessScriptPubKey: WitnessScriptPubKey): Try[ExecutedScriptProgram] = {
    scriptPubKeyExecutedProgram.txSignatureComponent match {
      case b: BaseTxSigComponent =>
        val scriptSig =
          scriptPubKeyExecutedProgram.txSignatureComponent.scriptSignature
        if (scriptSig != EmptyScriptSignature && !b.scriptPubKey
              .isInstanceOf[P2SHScriptPubKey]) {
          Success(
            ScriptProgram(scriptPubKeyExecutedProgram,
                          ScriptErrorWitnessMalleated))
        } else {
          witnessScriptPubKey.witnessVersion match {
            case WitnessVersion0 =>
              logger.error(
                "Cannot verify witness program with a BaseTxSigComponent")
              Success(
                ScriptProgram(scriptPubKeyExecutedProgram,
                              ScriptErrorWitnessProgramWitnessEmpty))
            case UnassignedWitness(_) =>
              evaluateUnassignedWitness(b)
          }
        }
      case w: WitnessTxSigComponent =>
        val scriptSig =
          scriptPubKeyExecutedProgram.txSignatureComponent.scriptSignature
        val (witnessVersion, witnessProgram) =
          (witnessScriptPubKey.witnessVersion,
           witnessScriptPubKey.witnessProgram)
        val witness = w.witness
        //scriptsig must be empty if we have raw p2wsh
        //if script pubkey is a P2SHScriptPubKey then we have P2SH(P2WSH)
        if (scriptSig != EmptyScriptSignature && !w.scriptPubKey
              .isInstanceOf[P2SHScriptPubKey]) {
          Success(
            ScriptProgram(scriptPubKeyExecutedProgram,
                          ScriptErrorWitnessMalleated))
        } else if (witness.stack.exists(_.size > maxPushSize)) {
          Success(
            ScriptProgram(scriptPubKeyExecutedProgram, ScriptErrorPushSize))
        } else {
          verifyWitnessProgram(witnessVersion, witness, witnessProgram, w)
        }
      case _: WitnessTxSigComponentRebuilt =>
        Failure(new IllegalArgumentException(
          "Cannot have a rebuild witness tx sig component here, the witness tx sigcomponent is rebuilt in verifyWitnessProgram"))
    }
  }

  /**
    * Verifies a segregated witness program by running it through the interpreter
    * [[https://github.com/bitcoin/bitcoin/blob/f8528134fc188abc5c7175a19680206964a8fade/src/script/interpreter.cpp#L1302]]
    */
  private def verifyWitnessProgram(
      witnessVersion: WitnessVersion,
      scriptWitness: ScriptWitness,
      witnessProgram: Seq[ScriptToken],
      wTxSigComponent: WitnessTxSigComponent): Try[ExecutedScriptProgram] = {

    /** Helper function to run the post segwit execution checks */
    def postSegWitProgramChecks(
        evaluated: ExecutedScriptProgram): ExecutedScriptProgram = {
      logger.trace("Stack after evaluating witness: " + evaluated.stack)
      if (evaluated.error.isDefined) evaluated
      else if (evaluated.stack.size != 1 || evaluated.stackTopIsFalse)
        ScriptProgram(evaluated, ScriptErrorEvalFalse)
      else evaluated
    }

    witnessVersion match {
      case WitnessVersion0 =>
        val either: Either[(Seq[ScriptToken], ScriptPubKey), ScriptError] =
          witnessVersion.rebuild(scriptWitness, witnessProgram)
        either match {
          case Left((stack, scriptPubKey)) =>
            val newWTxSigComponent =
              rebuildWTxSigComponent(wTxSigComponent, scriptPubKey)
            val newProgram = newWTxSigComponent.map { comp =>
              PreExecutionScriptProgram(
                txSignatureComponent = comp,
                stack = stack.toList,
                script = scriptPubKey.asm.toList,
                originalScript = scriptPubKey.asm.toList,
                altStack = Nil,
                flags = comp.flags)
            }
            val evaluated = newProgram.map(p => loop(p, 0))
            evaluated.map(e => postSegWitProgramChecks(e))
          case Right(err) =>
            val program = ExecutedScriptProgram(txSignatureComponent =
                                                  wTxSigComponent,
                                                stack = Nil,
                                                script = Nil,
                                                originalScript = Nil,
                                                altStack = Nil,
                                                flags = wTxSigComponent.flags,
                                                error = Some(err))
            Success(program)
        }
      case UnassignedWitness(_) =>
        evaluateUnassignedWitness(wTxSigComponent)
    }
  }

  /**
    * The execution loop for a script
    *
    * @param program the program whose script needs to be evaluated
    * @return program the final state of the program after being evaluated by the interpreter
    */
  @tailrec
  private def loop(
      program: ScriptProgram,
      opCount: Int): ExecutedScriptProgram = {
    logger.trace("Stack: " + program.stack)
    logger.trace("Script: " + program.script)
    val scriptByteVector = BitcoinSUtil.toByteVector(program.script)
    if (opCount > maxScriptOps && !program
          .isInstanceOf[ExecutedScriptProgram]) {
      logger.error(
        "We have reached the maximum amount of script operations allowed")
      logger.error(
        "Here are the remaining operations in the script: " + program.script)
      loop(ScriptProgram(program, ScriptErrorOpCount), opCount)
    } else if (scriptByteVector.length > 10000 && !program
                 .isInstanceOf[ExecutedScriptProgram]) {
      logger.error("We cannot run a script that is larger than 10,000 bytes")
      program match {
        case p: PreExecutionScriptProgram =>
          loop(ScriptProgram(ScriptProgram.toExecutionInProgress(p),
                             ScriptErrorScriptSize),
               opCount)
        case _: ExecutionInProgressScriptProgram | _: ExecutedScriptProgram =>
          loop(ScriptProgram(program, ScriptErrorScriptSize), opCount)
      }
    } else {
      program match {
        case p: PreExecutionScriptProgram =>
          loop(ScriptProgram.toExecutionInProgress(p, Some(p.stack)), opCount)
        case p: ExecutedScriptProgram =>
          val countedOps = program.originalScript
            .map(BitcoinScriptUtil.countsTowardsScriptOpLimit(_))
            .count(_ == true)
          logger.trace("Counted ops: " + countedOps)
          if (countedOps > maxScriptOps && p.error.isEmpty) {
            loop(ScriptProgram(p, ScriptErrorOpCount), opCount)
          } else p

        case p: ExecutionInProgressScriptProgram =>
          p.script match {
            //if at any time we see that the program is not valid
            //cease script execution
            case _ if p.script.intersect(Seq(OP_VERIF, OP_VERNOTIF)).nonEmpty =>
              logger.error(
                "Script is invalid even when a OP_VERIF or OP_VERNOTIF occurs in an unexecuted OP_IF branch")
              loop(ScriptProgram(p, ScriptErrorBadOpCode), opCount)
            //disabled splice operation
            case _
                if p.script
                  .intersect(Seq(OP_CAT, OP_SUBSTR, OP_LEFT, OP_RIGHT))
                  .nonEmpty =>
              logger.error(
                "Script is invalid because it contains a disabled splice operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode), opCount)
            //disabled bitwise operations
            case _
                if p.script
                  .intersect(Seq(OP_INVERT, OP_AND, OP_OR, OP_XOR))
                  .nonEmpty =>
              logger.error(
                "Script is invalid because it contains a disabled bitwise operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode), opCount)
            //disabled arithmetic operations
            case _
                if p.script
                  .intersect(
                    Seq(OP_MUL,
                        OP_2MUL,
                        OP_DIV,
                        OP_2DIV,
                        OP_MOD,
                        OP_LSHIFT,
                        OP_RSHIFT))
                  .nonEmpty =>
              logger.error(
                "Script is invalid because it contains a disabled arithmetic operation")
              loop(ScriptProgram(p, ScriptErrorDisabledOpCode), opCount)
            //program cannot contain a push operation > 520 bytes
            case _
                if (p.script.exists(token => token.bytes.size > maxPushSize)) =>
              logger.error(
                "We have a script constant that is larger than 520 bytes, this is illegal: " + p.script)
              loop(ScriptProgram(p, ScriptErrorPushSize), opCount)
            //program stack size cannot be greater than 1000 elements
            case _ if ((p.stack.size + p.altStack.size) > 1000) =>
              logger.error(
                "We cannot have a stack + alt stack size larger than 1000 elements")
              loop(ScriptProgram(p, ScriptErrorStackSize), opCount)

            //stack operations
            case OP_DUP :: _ =>
              loop(StackInterpreter.opDup(p), calcOpCount(opCount, OP_DUP))
            case OP_DEPTH :: _ =>
              loop(StackInterpreter.opDepth(p), calcOpCount(opCount, OP_DEPTH))
            case OP_TOALTSTACK :: _ =>
              loop(StackInterpreter.opToAltStack(p),
                   calcOpCount(opCount, OP_TOALTSTACK))
            case OP_FROMALTSTACK :: _ =>
              loop(StackInterpreter.opFromAltStack(p),
                   calcOpCount(opCount, OP_FROMALTSTACK))
            case OP_DROP :: _ =>
              loop(StackInterpreter.opDrop(p), calcOpCount(opCount, OP_DROP))
            case OP_IFDUP :: _ =>
              loop(StackInterpreter.opIfDup(p), calcOpCount(opCount, OP_IFDUP))
            case OP_NIP :: _ =>
              loop(StackInterpreter.opNip(p), calcOpCount(opCount, OP_NIP))
            case OP_OVER :: _ =>
              loop(StackInterpreter.opOver(p), calcOpCount(opCount, OP_OVER))
            case OP_PICK :: _ =>
              loop(StackInterpreter.opPick(p), calcOpCount(opCount, OP_PICK))
            case OP_ROLL :: _ =>
              loop(StackInterpreter.opRoll(p), calcOpCount(opCount, OP_ROLL))
            case OP_ROT :: _ =>
              loop(StackInterpreter.opRot(p), calcOpCount(opCount, OP_ROT))
            case OP_2ROT :: _ =>
              loop(StackInterpreter.op2Rot(p), calcOpCount(opCount, OP_2ROT))
            case OP_2DROP :: _ =>
              loop(StackInterpreter.op2Drop(p), calcOpCount(opCount, OP_2DROP))
            case OP_SWAP :: _ =>
              loop(StackInterpreter.opSwap(p), calcOpCount(opCount, OP_SWAP))
            case OP_TUCK :: _ =>
              loop(StackInterpreter.opTuck(p), calcOpCount(opCount, OP_TUCK))
            case OP_2DUP :: _ =>
              loop(StackInterpreter.op2Dup(p), calcOpCount(opCount, OP_2DUP))
            case OP_3DUP :: _ =>
              loop(StackInterpreter.op3Dup(p), calcOpCount(opCount, OP_3DUP))
            case OP_2OVER :: _ =>
              loop(StackInterpreter.op2Over(p), calcOpCount(opCount, OP_2OVER))
            case OP_2SWAP :: _ =>
              loop(StackInterpreter.op2Swap(p), calcOpCount(opCount, OP_2SWAP))

            //arithmetic operations
            case OP_ADD :: _ =>
              loop(ArithmeticInterpreter.opAdd(p), calcOpCount(opCount, OP_ADD))
            case OP_1ADD :: _ =>
              loop(ArithmeticInterpreter.op1Add(p),
                   calcOpCount(opCount, OP_1ADD))
            case OP_1SUB :: _ =>
              loop(ArithmeticInterpreter.op1Sub(p),
                   calcOpCount(opCount, OP_1SUB))
            case OP_SUB :: _ =>
              loop(ArithmeticInterpreter.opSub(p), calcOpCount(opCount, OP_SUB))
            case OP_ABS :: _ =>
              loop(ArithmeticInterpreter.opAbs(p), calcOpCount(opCount, OP_ABS))
            case OP_NEGATE :: _ =>
              loop(ArithmeticInterpreter.opNegate(p),
                   calcOpCount(opCount, OP_NEGATE))
            case OP_NOT :: _ =>
              loop(ArithmeticInterpreter.opNot(p), calcOpCount(opCount, OP_NOT))
            case OP_0NOTEQUAL :: _ =>
              loop(ArithmeticInterpreter.op0NotEqual(p),
                   calcOpCount(opCount, OP_0NOTEQUAL))
            case OP_BOOLAND :: _ =>
              loop(ArithmeticInterpreter.opBoolAnd(p),
                   calcOpCount(opCount, OP_BOOLAND))
            case OP_BOOLOR :: _ =>
              loop(ArithmeticInterpreter.opBoolOr(p),
                   calcOpCount(opCount, OP_BOOLOR))
            case OP_NUMEQUAL :: _ =>
              loop(ArithmeticInterpreter.opNumEqual(p),
                   calcOpCount(opCount, OP_NUMEQUAL))
            case OP_NUMEQUALVERIFY :: _ =>
              loop(ArithmeticInterpreter.opNumEqualVerify(p),
                   calcOpCount(opCount, OP_NUMEQUALVERIFY))
            case OP_NUMNOTEQUAL :: _ =>
              loop(ArithmeticInterpreter.opNumNotEqual(p),
                   calcOpCount(opCount, OP_NUMNOTEQUAL))
            case OP_LESSTHAN :: _ =>
              loop(ArithmeticInterpreter.opLessThan(p),
                   calcOpCount(opCount, OP_LESSTHAN))
            case OP_GREATERTHAN :: _ =>
              loop(ArithmeticInterpreter.opGreaterThan(p),
                   calcOpCount(opCount, OP_GREATERTHAN))
            case OP_LESSTHANOREQUAL :: _ =>
              loop(ArithmeticInterpreter.opLessThanOrEqual(p),
                   calcOpCount(opCount, OP_LESSTHANOREQUAL))
            case OP_GREATERTHANOREQUAL :: _ =>
              loop(ArithmeticInterpreter.opGreaterThanOrEqual(p),
                   calcOpCount(opCount, OP_GREATERTHANOREQUAL))
            case OP_MIN :: _ =>
              loop(ArithmeticInterpreter.opMin(p), calcOpCount(opCount, OP_MIN))
            case OP_MAX :: _ =>
              loop(ArithmeticInterpreter.opMax(p), calcOpCount(opCount, OP_MAX))
            case OP_WITHIN :: _ =>
              loop(ArithmeticInterpreter.opWithin(p),
                   calcOpCount(opCount, OP_WITHIN))

            //bitwise operations
            case OP_EQUAL :: _ =>
              loop(BitwiseInterpreter.opEqual(p),
                   calcOpCount(opCount, OP_EQUAL))

            case OP_EQUALVERIFY :: _ =>
              loop(BitwiseInterpreter.opEqualVerify(p),
                   calcOpCount(opCount, OP_EQUALVERIFY))

            case OP_0 :: t =>
              loop(ScriptProgram(p, ScriptNumber.zero :: p.stack, t),
                   calcOpCount(opCount, OP_0))
            case (scriptNumberOp: ScriptNumberOperation) :: t =>
              loop(ScriptProgram(p,
                                 ScriptNumber(scriptNumberOp.toLong) :: p.stack,
                                 t),
                   calcOpCount(opCount, scriptNumberOp))
            case (bytesToPushOntoStack: BytesToPushOntoStack) :: _ =>
              loop(ConstantInterpreter.pushScriptNumberBytesToStack(p),
                   calcOpCount(opCount, bytesToPushOntoStack))
            case (scriptNumber: ScriptNumber) :: t =>
              loop(ScriptProgram(p, scriptNumber :: p.stack, t),
                   calcOpCount(opCount, scriptNumber))
            case OP_PUSHDATA1 :: _ =>
              loop(ConstantInterpreter.opPushData1(p),
                   calcOpCount(opCount, OP_PUSHDATA1))
            case OP_PUSHDATA2 :: _ =>
              loop(ConstantInterpreter.opPushData2(p),
                   calcOpCount(opCount, OP_PUSHDATA2))
            case OP_PUSHDATA4 :: _ =>
              loop(ConstantInterpreter.opPushData4(p),
                   calcOpCount(opCount, OP_PUSHDATA4))

            case (x: ScriptConstant) :: t =>
              loop(ScriptProgram(p, x :: p.stack, t), calcOpCount(opCount, x))

            //control operations
            case OP_IF :: _ =>
              loop(ControlOperationsInterpreter.opIf(p),
                   calcOpCount(opCount, OP_IF))
            case OP_NOTIF :: _ =>
              loop(ControlOperationsInterpreter.opNotIf(p),
                   calcOpCount(opCount, OP_NOTIF))
            case OP_ELSE :: _ =>
              loop(ControlOperationsInterpreter.opElse(p),
                   calcOpCount(opCount, OP_ELSE))
            case OP_ENDIF :: _ =>
              loop(ControlOperationsInterpreter.opEndIf(p),
                   calcOpCount(opCount, OP_ENDIF))
            case OP_RETURN :: _ =>
              loop(ControlOperationsInterpreter.opReturn(p),
                   calcOpCount(opCount, OP_RETURN))

            case OP_VERIFY :: _ =>
              loop(ControlOperationsInterpreter.opVerify(p),
                   calcOpCount(opCount, OP_VERIFY))

            //crypto operations
            case OP_HASH160 :: _ =>
              loop(CryptoInterpreter.opHash160(p),
                   calcOpCount(opCount, OP_HASH160))
            case OP_CHECKSIG :: _ =>
              loop(CryptoInterpreter.opCheckSig(p),
                   calcOpCount(opCount, OP_CHECKSIG))
            case OP_CHECKSIGVERIFY :: _ =>
              loop(CryptoInterpreter.opCheckSigVerify(p),
                   calcOpCount(opCount, OP_CHECKSIGVERIFY))
            case OP_SHA1 :: _ =>
              loop(CryptoInterpreter.opSha1(p), calcOpCount(opCount, OP_SHA1))
            case OP_RIPEMD160 :: _ =>
              loop(CryptoInterpreter.opRipeMd160(p),
                   calcOpCount(opCount, OP_RIPEMD160))
            case OP_SHA256 :: _ =>
              loop(CryptoInterpreter.opSha256(p),
                   calcOpCount(opCount, OP_SHA256))
            case OP_HASH256 :: _ =>
              loop(CryptoInterpreter.opHash256(p),
                   calcOpCount(opCount, OP_HASH256))
            case OP_CODESEPARATOR :: _ =>
              loop(CryptoInterpreter.opCodeSeparator(p),
                   calcOpCount(opCount, OP_CODESEPARATOR))
            case OP_CHECKMULTISIG :: _ =>
              CryptoInterpreter.opCheckMultiSig(p) match {
                case newProgram: ExecutedScriptProgram =>
                  //script was marked invalid for other reasons, don't need to update the opcount
                  loop(newProgram, opCount)
                case newProgram @ (_: ExecutionInProgressScriptProgram |
                    _: PreExecutionScriptProgram) =>
                  val newOpCount = calcOpCount(opCount, OP_CHECKMULTISIG) + BitcoinScriptUtil
                    .numPossibleSignaturesOnStack(program)
                    .toInt
                  loop(newProgram, newOpCount)
              }
            case OP_CHECKMULTISIGVERIFY :: _ =>
              CryptoInterpreter.opCheckMultiSigVerify(p) match {
                case newProgram: ExecutedScriptProgram =>
                  //script was marked invalid for other reasons, don't need to update the opcount
                  loop(newProgram, opCount)
                case newProgram @ (_: ExecutionInProgressScriptProgram |
                    _: PreExecutionScriptProgram) =>
                  val newOpCount = calcOpCount(opCount, OP_CHECKMULTISIGVERIFY) + BitcoinScriptUtil
                    .numPossibleSignaturesOnStack(program)
                    .toInt
                  loop(newProgram, newOpCount)
              }
            //reserved operations
            case OP_NOP :: t =>
              //script discourage upgradeable flag does not apply to a OP_NOP
              loop(ScriptProgram(p, p.stack, t), calcOpCount(opCount, OP_NOP))

            //if we see an OP_NOP and the DISCOURAGE_UPGRADABLE_OP_NOPS flag is set we must fail our program
            case (nop: NOP) :: _
                if ScriptFlagUtil.discourageUpgradableNOPs(p.flags) =>
              logger.error(
                "We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
              loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),
                   calcOpCount(opCount, nop))
            case (nop: NOP) :: t =>
              loop(ScriptProgram(p, p.stack, t), calcOpCount(opCount, nop))
            case OP_RESERVED :: _ =>
              logger.error(
                "OP_RESERVED automatically marks transaction invalid")
              loop(ScriptProgram(p, ScriptErrorBadOpCode),
                   calcOpCount(opCount, OP_RESERVED))
            case OP_VER :: _ =>
              logger.error("Transaction is invalid when executing OP_VER")
              loop(ScriptProgram(p, ScriptErrorBadOpCode),
                   calcOpCount(opCount, OP_VER))
            case OP_RESERVED1 :: _ =>
              logger.error("Transaction is invalid when executing OP_RESERVED1")
              loop(ScriptProgram(p, ScriptErrorBadOpCode),
                   calcOpCount(opCount, OP_RESERVED1))
            case OP_RESERVED2 :: _ =>
              logger.error("Transaction is invalid when executing OP_RESERVED2")
              loop(ScriptProgram(p, ScriptErrorBadOpCode),
                   calcOpCount(opCount, OP_RESERVED2))

            case (reservedOperation: ReservedOperation) :: _ =>
              logger.error(
                "Undefined operation found which automatically fails the script: " + reservedOperation)
              loop(ScriptProgram(p, ScriptErrorBadOpCode),
                   calcOpCount(opCount, reservedOperation))
            //splice operations
            case OP_SIZE :: _ =>
              loop(SpliceInterpreter.opSize(p), calcOpCount(opCount, OP_SIZE))

            //locktime operations
            case OP_CHECKLOCKTIMEVERIFY :: _ =>
              //check if CLTV is enforced yet
              if (ScriptFlagUtil.checkLockTimeVerifyEnabled(p.flags)) {
                loop(LockTimeInterpreter.opCheckLockTimeVerify(p),
                     calcOpCount(opCount, OP_CHECKLOCKTIMEVERIFY))
              } //if not, check to see if we should discourage p
              else if (ScriptFlagUtil.discourageUpgradableNOPs(p.flags)) {
                logger.error(
                  "We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
                loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),
                     calcOpCount(opCount, OP_CHECKLOCKTIMEVERIFY))
              } //in this case, just reat OP_CLTV just like a NOP and remove it from the stack
              else
                loop(ScriptProgram(p, p.script.tail, ScriptProgram.Script),
                     calcOpCount(opCount, OP_CHECKLOCKTIMEVERIFY))
            case OP_CHECKSEQUENCEVERIFY :: _ =>
              //check if CLTV is enforced yet
              if (ScriptFlagUtil.checkSequenceVerifyEnabled(p.flags)) {
                loop(LockTimeInterpreter.opCheckSequenceVerify(p),
                     calcOpCount(opCount, OP_CHECKSEQUENCEVERIFY))
              } //if not, check to see if we should discourage p
              else if (ScriptFlagUtil.discourageUpgradableNOPs(p.flags)) {
                logger.error(
                  "We cannot execute a NOP when the ScriptVerifyDiscourageUpgradableNOPs is set")
                loop(ScriptProgram(p, ScriptErrorDiscourageUpgradableNOPs),
                     calcOpCount(opCount, OP_CHECKSEQUENCEVERIFY))
              } //in this case, just read OP_CSV just like a NOP and remove it from the stack
              else
                loop(ScriptProgram(p, p.script.tail, ScriptProgram.Script),
                     calcOpCount(opCount, OP_CHECKSEQUENCEVERIFY))
            //no more script operations to run, return whether the program is valid and the final state of the program
            case Nil    => loop(ScriptProgram.toExecutedProgram(p), opCount)
            case h :: _ => throw new RuntimeException(h + " was unmatched")
          }
      }
    }
  }

  /**
    * Checks the validity of a transaction in accordance to bitcoin core's CheckTransaction function
    * https://github.com/bitcoin/bitcoin/blob/f7a21dae5dbf71d5bc00485215e84e6f2b309d0a/src/main.cpp#L939.
    */
  def checkTransaction(transaction: Transaction): Boolean = {
    val inputOutputsNotZero =
      !(transaction.inputs.isEmpty || transaction.outputs.isEmpty)
    val txNotLargerThanBlock = transaction.bytes.size < Consensus.maxBlockSize
    val outputsSpendValidAmountsOfMoney = !transaction.outputs.exists(o =>
      o.value < CurrencyUnits.zero || o.value > Consensus.maxMoney)

    val outputValues = transaction.outputs.map(_.value)
    val totalSpentByOutputs: CurrencyUnit =
      outputValues.fold(CurrencyUnits.zero)(_ + _)
    val allOutputsValidMoneyRange = validMoneyRange(totalSpentByOutputs)
    val prevOutputTxIds = transaction.inputs.map(_.previousOutput.txId)
    val noDuplicateInputs = prevOutputTxIds.distinct.size == prevOutputTxIds.size

    val isValidScriptSigForCoinbaseTx = transaction.isCoinbase match {
      case true =>
        transaction.inputs.head.scriptSignature.asmBytes.size >= 2 &&
          transaction.inputs.head.scriptSignature.asmBytes.size <= 100
      case false =>
        //since this is not a coinbase tx we cannot have any empty previous outs inside of inputs
        !transaction.inputs.exists(_.previousOutput == EmptyTransactionOutPoint)
    }
    inputOutputsNotZero && txNotLargerThanBlock && outputsSpendValidAmountsOfMoney && noDuplicateInputs &&
    allOutputsValidMoneyRange && noDuplicateInputs && isValidScriptSigForCoinbaseTx
  }

  /** Determines if the given currency unit is within the valid range for the system */
  def validMoneyRange(currencyUnit: CurrencyUnit): Boolean = {
    currencyUnit >= CurrencyUnits.zero && currencyUnit <= Consensus.maxMoney
  }

  /**  Calculates the new op count after the execution of the given
    * [[org.bitcoins.core.script.constant.ScriptToken ScriptToken]] */
  private def calcOpCount(oldOpCount: Int, token: ScriptToken): Int =
    BitcoinScriptUtil.countsTowardsScriptOpLimit(token) match {
      case true  => oldOpCount + 1
      case false => oldOpCount
    }

  /**
    * Checks if the transaction contained a witness that we did not use
    * [[https://github.com/bitcoin/bitcoin/blob/528472111b4965b1a99c4bcf08ac5ec93d87f10f/src/script/interpreter.cpp#L1515-L1523]]
    * Return true if witness was NOT used, return false if witness was used.
    */
  private def hasUnexpectedWitness(program: ScriptProgram): Boolean = {
    val txSigComponent = program.txSignatureComponent
    val unexpectedWitness = txSigComponent match {
      case b: BaseTxSigComponent =>
        b.transaction match {
          case wtx: WitnessTransaction =>
            wtx.witness
              .witnesses(txSigComponent.inputIndex.toInt)
              .stack
              .nonEmpty
          case _: BaseTransaction => false
        }
      case _: WitnessTxSigComponentRaw => false
      case w: WitnessTxSigComponentP2SH =>
        !w.scriptSignature.redeemScript.isInstanceOf[WitnessScriptPubKey]
      case r: WitnessTxSigComponentRebuilt =>
        r.transaction.witness
          .witnesses(txSigComponent.inputIndex.toInt)
          .stack
          .nonEmpty
    }

    if (unexpectedWitness)
      logger.error(
        "Found unexpected witness that was not used by the ScriptProgram: " + program)
    unexpectedWitness
  }

  /**
    * Helper function used to rebuild a
    * [[org.bitcoins.core.crypto.WitnessTxSigComponentRebuilt WitnessTxSigComponentRebuilt]]
    * this converts a [[org.bitcoins.core.protocol.script.WitnessScriptPubKey WitnessScriptPubKey]]
    * into it's corresponding [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
    */
  private def rebuildWTxSigComponent(
      old: WitnessTxSigComponent,
      rebuildScriptPubKey: ScriptPubKey): Try[WitnessTxSigComponentRebuilt] = {
    val updatedOutput = TransactionOutput(old.output.value, rebuildScriptPubKey)
    old match {
      case wTxSigComponentRaw: WitnessTxSigComponentRaw =>
        Success(
          WitnessTxSigComponentRebuilt(old.transaction,
                                       old.inputIndex,
                                       updatedOutput,
                                       wTxSigComponentRaw.scriptPubKey,
                                       old.flags))
      case wTxSigComponentP2SH: WitnessTxSigComponentP2SH =>
        wTxSigComponentP2SH.witnessScriptPubKey.map {
          wit: WitnessScriptPubKey =>
            val updatedOutput =
              TransactionOutput(old.output.value, rebuildScriptPubKey)
            WitnessTxSigComponentRebuilt(old.transaction,
                                         old.inputIndex,
                                         updatedOutput,
                                         wit,
                                         old.flags)
        }
    }
  }

  /** Logic to evaluate a witnesss version that has not been assigned yet */
  private def evaluateUnassignedWitness(
      txSigComponent: TxSigComponent): Try[ExecutedScriptProgram] = {
    logger.warn("Unassigned witness inside of witness script pubkey")
    val flags = txSigComponent.flags
    val discourageUpgradableWitnessVersion =
      ScriptFlagUtil.discourageUpgradableWitnessProgram(flags)
    if (discourageUpgradableWitnessVersion) {
      val executed = ExecutedScriptProgram(
        txSignatureComponent = txSigComponent,
        stack = Nil,
        script = Nil,
        originalScript = txSigComponent.scriptPubKey.asm.toList,
        altStack = Nil,
        flags,
        error = Some(ScriptErrorDiscourageUpgradeableWitnessProgram)
      )
      Success(executed)
    } else {
      //if we are not discouraging upgradable ops, we just trivially return the program with an OP_TRUE on the stack
      //see: https://github.com/bitcoin/bitcoin/blob/b83264d9c7a8ddb79f64bd9540caddc8632ef31f/src/script/interpreter.cpp#L1386-L1389
      val inProgress = ExecutionInProgressScriptProgram(
        txSignatureComponent = txSigComponent,
        stack = List(OP_TRUE),
        script = Nil,
        originalScript = txSigComponent.scriptPubKey.asm.toList,
        altStack = Nil,
        flags = flags,
        lastCodeSeparator = None
      )
      val evaluated = loop(inProgress, 0)
      Success(evaluated)
    }
  }
}
object ScriptInterpreter extends ScriptInterpreter
