package org.bitcoins.core.script.locktime

import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.protocol.transaction.TransactionConstants
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.constant.{ ScriptConstant, ScriptNumber, ScriptToken }
import org.bitcoins.core.script.flag.ScriptFlagUtil
import org.bitcoins.core.script.result._
import org.bitcoins.core.util.BitcoinSLogger

import scala.annotation.tailrec
/**
 * Created by chris on 2/8/16.
 */
sealed abstract class LockTimeInterpreter {

  private def logger = BitcoinSLogger.logger
  /**
   * Marks transaction as invalid if the top stack item is greater than the transaction's nLockTime field,
   * otherwise script evaluation continues as though an OP_NOP was executed. Transaction is also invalid if
   * 1. the stack is empty; or
   * 2. the top stack item is negative; or
   * 3. the top stack item is greater than or equal to 500000000 while the transaction's nLockTime field is less than 500000000,
   * or vice versa; or
   * 4. the input's nSequence field is equal to 0xffffffff.
   * The precise semantics are described in BIP 0065
   */
  @tailrec
  final def opCheckLockTimeVerify(program: ScriptProgram): ScriptProgram = {
    require(
      program.script.headOption.contains(OP_CHECKLOCKTIMEVERIFY),
      "Script top must be OP_CHECKLOCKTIMEVERIFY"
    )
    val input = program.txSignatureComponent.transaction.inputs(program.txSignatureComponent.inputIndex.toInt)
    val transaction = program.txSignatureComponent.transaction
    if (program.stack.size == 0) {
      logger.error("Transaction validation failing in OP_CHECKLOCKTIMEVERIFY because we have no stack items")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    } else if (input.sequence == TransactionConstants.sequence) {
      logger.error("Transaction validation failing in OP_CHECKLOCKTIMEVERIFY because the sequence number is 0xffffffff")
      ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
    } else {
      program.stack.head match {
        case s: ScriptNumber if (s < ScriptNumber.zero) =>
          logger.error("OP_CHECKLOCKTIMEVERIFY marks tx as invalid if the stack top is negative")
          ScriptProgram(program, ScriptErrorNegativeLockTime)
        case s: ScriptNumber if (s >= ScriptNumber(500000000) && transaction.lockTime < UInt32(500000000)) =>
          logger.error("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top >= 500000000 & tx locktime < 500000000")
          ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
        case s: ScriptNumber if (s < ScriptNumber(500000000) && transaction.lockTime >= UInt32(500000000)) =>
          logger.error("OP_CHECKLOCKTIMEVERIFY marks the tx as invalid if stack top < 500000000 & tx locktime >= 500000000")
          ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
        case s: ScriptNumber =>
          if (s.bytes.size > 5) {
            //if the number size is larger than 5 bytes the number is invalid
            ScriptProgram(program, ScriptErrorUnknownError)
          } else if (checkLockTime(program, s)) {
            ScriptProgram(program, program.script.tail, ScriptProgram.Script)
          } else {
            logger.error("Stack top locktime and transaction locktime number comparison failed")
            ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
          }
        case s: ScriptConstant =>
          opCheckLockTimeVerify(ScriptProgram(program, ScriptNumber(s.hex) :: program.stack.tail, ScriptProgram.Stack))
        case _: ScriptToken => ScriptProgram(program, ScriptErrorUnknownError)
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
   * [[https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki]]
   */
  @tailrec
  final def opCheckSequenceVerify(program: ScriptProgram): ScriptProgram = {
    if (program.stack.isEmpty) {
      logger.error("Cannot execute OP_CHECKSEQUENCEVERIFY on an empty stack")
      ScriptProgram(program, ScriptErrorInvalidStackOperation)
    } else {
      program.stack.head match {
        case ScriptNumber.negativeOne => ScriptProgram(program, ScriptErrorNegativeLockTime)
        case s: ScriptNumber if (ScriptFlagUtil.requireMinimalData(program.flags) && !s.isShortestEncoding) =>
          logger.error("Sequence number is not encoded in the shortest way possible")
          ScriptProgram(program, ScriptErrorUnknownError)
        case s: ScriptNumber if (!isLockTimeBitOff(s)) =>
          //see BIP68 for semantic of locktimeDisableFlag
          logger.info("Locktime disable flag was set so OP_CHECKSEQUENCEVERIFY is treated as a NOP")
          ScriptProgram(program, program.script.tail, ScriptProgram.Script)
        case s: ScriptNumber if (isLockTimeBitOff(s) && program.txSignatureComponent.transaction.version < UInt32(2)) =>
          logger.error("OP_CSV fails if locktime bit is not set and the tx version < 2")
          ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
        case s: ScriptNumber =>
          if (s.bytes.size > 5) {
            //if the number size is larger than 5 bytes the number is invalid
            logger.error("The OP_CSV value in the script was larger than 5 bytes in size.")
            ScriptProgram(program, ScriptErrorUnknownError)
          } else if (checkSequence(program, s)) {
            ScriptProgram(program, program.stack, program.script.tail)
          } else {
            ScriptProgram(program, ScriptErrorUnsatisfiedLocktime)
          }
        case s: ScriptConstant =>
          opCheckSequenceVerify(ScriptProgram(program, ScriptNumber(s.hex) :: program.stack.tail, ScriptProgram.Stack))
        case token: ScriptToken =>
          throw new RuntimeException("Stack top must be either a ScriptConstant or a ScriptNumber, we got: " + token)

      }
    }
  }

  /**
   * Mimics this function inside of bitcoin core
   * [[https://github.com/bitcoin/bitcoin/blob/e26b62093ae21e89ed7d36a24a6b863f38ec631d/src/script/interpreter.cpp#L1196]]
   * [[https://github.com/bitcoin/bips/blob/master/bip-0068.mediawiki#specification]]
   * @param program the program whose transaction input's sequence is being compared
   * @param nSequence the script number on the stack top to compare to the input's sequence number
   * @return if the given script number is valid or not
   */
  def checkSequence(program: ScriptProgram, nSequence: ScriptNumber): Boolean = {
    val inputIndex = program.txSignatureComponent.inputIndex.toInt
    logger.debug("inputIndex: " + inputIndex)
    val transaction = program.txSignatureComponent.transaction

    // Relative lock times are supported by comparing the passed
    // in operand to the sequence number of the input.
    val txToSequence: UInt32 = transaction.inputs(inputIndex).sequence

    // Fail if the transaction's version number is not set high
    // enough to trigger BIP 68 rules.
    if (program.txSignatureComponent.transaction.version < UInt32(2)) {
      logger.error("OP_CSV fails the script if the transaction's version is less than 2.")
      return false
    }

    // Sequence numbers with their most significant bit set are not
    // consensus constrained. Testing that the transaction's sequence
    // number do not have this bit set prevents using this property
    // to get around a CHECKSEQUENCEVERIFY check.
    if (!isLockTimeBitOff(Int64(txToSequence.toLong))) return false

    val (nSequenceMasked, txToSequenceMasked) = (maskScriptNumber(nSequence), maskSequenceNumber(txToSequence))

    logger.debug("tx sequence number: " + transaction.inputs(inputIndex).sequence)
    logger.debug("txToSequenceMasked: " + txToSequenceMasked)
    logger.debug("nSequence: " + nSequence)
    logger.debug("nSequenceMasked: " + nSequenceMasked)
    logger.debug("Sequence locktime flag: " + TransactionConstants.sequenceLockTimeTypeFlag)

    // There are two kinds of nSequence: lock-by-blockheight
    // and lock-by-blocktime, distinguished by whether
    // nSequenceMasked < CTxIn::SEQUENCE_LOCKTIME_TYPE_FLAG.
    //
    // We want to compare apples to apples, so fail the script
    // unless the type of nSequenceMasked being tested is the same as
    // the nSequenceMasked in the transaction.
    if (!(isCSVLockByBlockHeight(nSequence, txToSequence) || isCSVLockByRelativeLockTime(nSequence, txToSequence))) {
      logger.error("The txSequence and nSequence (OP_CSV value) are not of the same type (timestamp/block-height).")
      return false
    }

    // Now that we know we're comparing apples-to-apples, the
    // comparison is a simple numeric one.
    if (nSequenceMasked > Int64(txToSequenceMasked.toLong)) {
      logger.error("OP_CSV fails because relative locktime in transaction has not been met yet. " +
        "(OP_CSV value was greater than the txInput's sequence) script number: " + nSequenceMasked + " tx sequence no: " + txToSequenceMasked)
      return false
    }

    true
  }

  /**
   * Checks if the given [[ScriptNumber]] and [[UInt32]] are valid values for spending
   * a OP_CSV value by block height
   */
  def isCSVLockByBlockHeight(scriptNumber: ScriptNumber, sequence: UInt32): Boolean = {
    isCSVLockByBlockHeight(scriptNumber) && isCSVLockByBlockHeight(sequence)
  }

  def isCSVLockByBlockHeight(sequence: UInt32): Boolean = !isCSVLockByRelativeLockTime(sequence)

  def isCSVLockByBlockHeight(scriptNumber: ScriptNumber): Boolean = !isCSVLockByRelativeLockTime(scriptNumber)

  /**
   * Checks if the given [[ScriptNumber]] and [[UInt32]] are valid values
   * for spending an OP_CSV value by time based relative lock time
   */
  def isCSVLockByRelativeLockTime(number: ScriptNumber, sequence: UInt32): Boolean = {
    isCSVLockByRelativeLockTime(number) && isCSVLockByRelativeLockTime(sequence)
  }

  def isCSVLockByRelativeLockTime(scriptNumber: ScriptNumber): Boolean = {
    (TransactionConstants.sequenceLockTimeTypeFlag.toLong &
      scriptNumber.toLong) == TransactionConstants.sequenceLockTimeTypeFlag.toLong
  }

  def isCSVLockByRelativeLockTime(sequence: UInt32): Boolean = {
    val result = (TransactionConstants.sequenceLockTimeTypeFlag &
      sequence) == TransactionConstants.sequenceLockTimeTypeFlag
    result
  }

  /** Masks the given [[ScriptNumber]] according to BIP112 */
  def maskScriptNumber(scriptNumber: ScriptNumber): ScriptNumber = {
    val nSequenceMasked: ScriptNumber = scriptNumber & Int64(TransactionConstants.fullSequenceLockTimeMask.toLong)
    nSequenceMasked
  }

  def maskSequenceNumber(sequence: UInt32): Int64 = {
    val txToSequenceMasked: Int64 = Int64((sequence & TransactionConstants.fullSequenceLockTimeMask).toLong)
    txToSequenceMasked
  }

  /**
   * Mimics this function inside of bitcoin core for checking the locktime of a transaction
   * [[https://github.com/bitcoin/bitcoin/blob/master/src/script/interpreter.cpp#L1160]].
   */
  private def checkLockTime(program: ScriptProgram, locktime: ScriptNumber): Boolean = {
    // There are two kinds of nLockTime: lock-by-blockheight
    // and lock-by-blocktime, distinguished by whether
    // nLockTime < LOCKTIME_THRESHOLD.
    //
    // We want to compare apples to apples, so fail the script
    // unless the type of nLockTime being tested is the same as
    // the nLockTime in the transaction.
    val transaction = program.txSignatureComponent.transaction
    val input = transaction.inputs(program.txSignatureComponent.inputIndex.toInt)
    if (!(
      (transaction.lockTime < TransactionConstants.locktimeThreshold &&
        locktime.toLong < TransactionConstants.locktimeThreshold.toLong) ||
        (transaction.lockTime >= TransactionConstants.locktimeThreshold &&
          locktime.toLong >= TransactionConstants.locktimeThreshold.toLong)
    )) return false

    // Now that we know we're comparing apples-to-apples, the
    // comparison is a simple numeric one.
    if (locktime > Int64(transaction.lockTime.toLong)) return false

    // Finally the nLockTime feature can be disabled and thus
    // CHECKLOCKTIMEVERIFY bypassed if every txin has been
    // finalized by setting nSequence to maxint. The
    // transaction would be allowed into the blockchain, making
    // the opcode ineffective.
    //
    // Testing if this vin is not final is sufficient to
    // prevent this condition. Alternatively we could test all
    // inputs, but testing just this input minimizes the data
    // required to prove correct CHECKLOCKTIMEVERIFY execution.
    if (input.sequence == TransactionConstants.sequence) {
      false
    } else true
  }

  /** The [[ScriptNumber]] on the stack has the disable flag (1 << 31) unset. */
  def isLockTimeBitOff(s: ScriptNumber): Boolean = (s.toLong & TransactionConstants.locktimeDisabledFlag.toLong) == 0

  def isLockTimeBitOff(num: Int64): Boolean = isLockTimeBitOff(ScriptNumber(num.hex))
}

object LockTimeInterpreter extends LockTimeInterpreter