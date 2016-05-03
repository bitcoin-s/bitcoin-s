package org.bitcoins.script.locktime


import org.bitcoins.protocol.transaction.TransactionConstants
import org.bitcoins.script.constant.{ScriptConstant, ScriptNumber, ScriptToken}
import org.bitcoins.script.result._
import org.bitcoins.script.ScriptProgram
import org.bitcoins.script.flag.ScriptFlagUtil
import org.bitcoins.util.BitcoinSLogger

import scala.annotation.tailrec
/**
 * Created by chris on 2/8/16.
 */
trait LockTimeInterpreter extends BitcoinSLogger {


  /**
   * Marks transaction as invalid if the top stack item is greater than the transaction's nLockTime field,
   * otherwise script evaluation continues as though an OP_NOP was executed. Transaction is also invalid if
   * 1. the stack is empty; or
   * 2. the top stack item is negative; or
   * 3. the top stack item is greater than or equal to 500000000 while the transaction's nLockTime field is less than 500000000,
   * or vice versa; or
   * 4. the input's nSequence field is equal to 0xffffffff.
   * The precise semantics are described in BIP 0065
 *
   * @param program
   * @return
   */
  def opCheckLockTimeVerify(program : ScriptProgram) : ScriptProgram = {
    require(program.script.headOption.isDefined && program.script.head == OP_CHECKLOCKTIMEVERIFY,
      "Script top must be OP_CHECKLOCKTIMEVERIFY")
    if (program.stack.size == 0) {
      logger.error("Transaction validation failing in OP_CHECKLOCKTIMEVERIFY because we have no stack items")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    } else if (program.txSignatureComponent.transaction.inputs(program.txSignatureComponent.inputIndex).sequence == TransactionConstants.sequence) {
      logger.error("Transaction validation failing in OP_CHECKLOCKTIMEVERIFY because the sequence number is 0xffffffff")
      ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
    }
    else {
      val isError : Option[ScriptError] = program.stack.head match {
        case s : ScriptNumber if (s < ScriptNumber.zero) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks tx as invalid if the stack top is negative")
          Some(ScriptErrorNegativeLockTime)
        case s : ScriptNumber if (s >= ScriptNumber(500000000) && program.txSignatureComponent.transaction.lockTime < 500000000) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top >= 500000000 & tx locktime < 500000000")
          Some(ScriptErrorUnsatisfiedLocktime)
        case s : ScriptNumber if (s < ScriptNumber(500000000) && program.txSignatureComponent.transaction.lockTime >= 500000000) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top < 500000000 & tx locktime >= 500000000")
          Some(ScriptErrorUnsatisfiedLocktime)
        case _ : ScriptNumber => None
        case _ : ScriptToken => Some(ScriptErrorUnknownError)
      }
      if (isError.isDefined) ScriptProgram(program,isError.get)
      else ScriptProgram(program,program.stack, program.script.tail)
    }
  }

  /**
    * When executed, if any of the following conditions are true, the script interpreter will terminate with an error:
    * 1.) the stack is empty; or
    * 2.) the top item on the stack is less than 0; or
    * 3.) the top item on the stack has the disable flag (1 << 31) unset; and
    *       the transaction version is less than 2; or
    *       the transaction input sequence number disable flag (1 << 31) is set; or
    *       the relative lock-time type is not the same; or
    *       the top stack item is greater than the transaction sequence (when masked according to the BIP68);
    * Otherwise, script execution will continue as if a NOP had been executed.
    * See BIP112 for more information
    * https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki
    * @param program
    * @return
    */
  @tailrec
  final def opCheckSequenceVerify(program : ScriptProgram) : ScriptProgram = {
    if (program.stack.isEmpty) {
      logger.error("Cannot execute OP_CHECKSEQUENCEVERIFY on an empty stack")
      ScriptProgram(program,ScriptErrorInvalidStackOperation)
    } else {
      program.stack.head match {
        case ScriptNumber.negativeOne => ScriptProgram(program,ScriptErrorNegativeLockTime)
        case s : ScriptNumber if (ScriptFlagUtil.requireMinimalData(program.flags) && !s.isShortestEncoding) =>
          logger.error("Sequence number is not encoded in the shortest way possible")
          ScriptProgram(program,ScriptErrorUnknownError)
        case s : ScriptNumber if (!isLockTimeBitOff(s)) =>
          //see BIP68 for semantic of locktimeDisableFalg
          logger.info("Locktime disable flag was set so OP_CHECKSEQUENCEVERIFY is treated as a NOP")
          ScriptProgram(program,program.script.tail,ScriptProgram.Script)
        case s : ScriptNumber if (isLockTimeBitOff(s) && program.txSignatureComponent.transaction.version < 2) =>
          logger.error("OP_CSV fails if locktime bit is not set and the tx version < 2")
          ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
        case s : ScriptConstant =>
          opCheckSequenceVerify(ScriptProgram(program, ScriptNumber(s.hex) :: program.stack.tail, ScriptProgram.Stack))
        case _ : ScriptToken => ScriptProgram(program, program.stack.tail, program.script.tail)
      }
    }

  }

  /**
    * If bit (1 << 31) of the sequence number is set,
    * then no consensus meaning is applied to the sequence number and can be included
    * in any block under all currently possible circumstances.
    * @return the mask that ben used with a bitwise and to indicate if the sequence number has any meaning
    */
  def locktimeDisabledFlag = 1L << 31

  /**
    * The script number on the stack has the disable flag (1 << 31) unset
    * @param s
    * @return
    */
  def isLockTimeBitOff(s : ScriptNumber) : Boolean = (s.num & locktimeDisabledFlag) == 0

}
