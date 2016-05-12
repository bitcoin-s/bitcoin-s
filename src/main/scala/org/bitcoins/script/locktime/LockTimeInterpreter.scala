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
   * @param program
   * @return
   */
  @tailrec
  final def opCheckLockTimeVerify(program : ScriptProgram) : ScriptProgram = {
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
      program.stack.head match {
        case s : ScriptNumber if (s < ScriptNumber.zero) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks tx as invalid if the stack top is negative")
          ScriptProgram(program,ScriptErrorNegativeLockTime)
        case s : ScriptNumber if (s >= ScriptNumber(500000000) && program.txSignatureComponent.transaction.lockTime < 500000000) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top >= 500000000 & tx locktime < 500000000")
          ScriptProgram(program,ScriptErrorUnsatisfiedLocktime)
        case s : ScriptNumber if (s < ScriptNumber(500000000) && program.txSignatureComponent.transaction.lockTime >= 500000000) =>
          logger.warn("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top < 500000000 & tx locktime >= 500000000")
          ScriptProgram(program,ScriptErrorUnsatisfiedLocktime)
        case _ : ScriptNumber => ScriptProgram(program,program.stack, program.script.tail)
        case s : ScriptConstant =>
          opCheckLockTimeVerify(ScriptProgram(program, ScriptNumber(s.hex) :: program.stack.tail, ScriptProgram.Stack))
        case _ : ScriptToken => ScriptProgram(program,ScriptErrorUnknownError)
      }
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
        case s : ScriptNumber =>
          if (checkSequence(program,s)) {
            ScriptProgram(program, program.stack.tail, program.script.tail)
          } else {
            logger.error("Stack top sequence and transaction input's sequence number comparison failed")
            ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
          }
        case s : ScriptConstant =>
          opCheckSequenceVerify(ScriptProgram(program, ScriptNumber(s.hex) :: program.stack.tail, ScriptProgram.Stack))
        case token : ScriptToken =>
          throw new RuntimeException("Stack top must be either a ScriptConstant or a ScriptNumber, we got: " + token)

      }
    }

  }

  /**
    * Mimics this function inside of bitcoin core
    * https://github.com/bitcoin/bitcoin/blob/e26b62093ae21e89ed7d36a24a6b863f38ec631d/src/script/interpreter.cpp#L1196
    * @param program the program whose transaction input's sequence is being compared
    * @param nSequence the script number on the stack top to compare to the input's sequence number
    * @return if the given script number is valid or not
    */
  def checkSequence(program : ScriptProgram, nSequence : ScriptNumber) : Boolean = {
    val inputIndex = program.txSignatureComponent.inputIndex
    val txToSequence : ScriptNumber = ScriptNumber(program.txSignatureComponent.transaction.inputs(inputIndex).sequence)

    if (program.txSignatureComponent.transaction.version < 2) return false

    val nLockTimeMask : Long = TransactionConstants.sequenceLockTimeTypeFlag | TransactionConstants.sequenceLockTimeMask
    val txToSequenceMasked : ScriptNumber = txToSequence & ScriptNumber(nLockTimeMask)

    val nSequenceMasked : ScriptNumber = nSequence & ScriptNumber(nLockTimeMask)

    // There are two kinds of nSequence: lock-by-blockheight
    // and lock-by-blocktime, distinguished by whether
    // nSequenceMasked < CTxIn::SEQUENCE_LOCKTIME_TYPE_FLAG.
    //
    // We want to compare apples to apples, so fail the script
    // unless the type of nSequenceMasked being tested is the same as
    // the nSequenceMasked in the transaction.
    if (!(
      (txToSequenceMasked <  ScriptNumber(TransactionConstants.sequenceLockTimeTypeFlag) &&
        nSequenceMasked < ScriptNumber(TransactionConstants.sequenceLockTimeTypeFlag)) ||
        (txToSequenceMasked >= ScriptNumber(TransactionConstants.sequenceLockTimeTypeFlag) &&
          nSequenceMasked >= ScriptNumber(TransactionConstants.sequenceLockTimeTypeFlag))
      )) return false

    // Now that we know we're comparing apples-to-apples, the
    // comparison is a simple numeric one.
    if (nSequenceMasked > txToSequenceMasked) return false

    true
  }

  /**
    * The script number on the stack has the disable flag (1 << 31) unset
    * @param s
    * @return
    */
  def isLockTimeBitOff(s : ScriptNumber) : Boolean = (s.num & TransactionConstants.locktimeDisabledFlag) == 0
}
