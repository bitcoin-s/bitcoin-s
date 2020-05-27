package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  CSVScriptPubKey,
  EmptyScriptSignature
}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.core.wallet.utxo.{
  ConditionalInputInfo,
  EmptyInputInfo,
  InputInfo,
  InputSigningInfo,
  LockTimeInputInfo,
  MultiSignatureInputInfo,
  P2PKHInputInfo,
  P2PKInputInfo,
  P2PKWithTimeoutInputInfo,
  P2SHInputInfo,
  P2WPKHV0InputInfo,
  P2WSHV0InputInfo,
  UnassignedSegwitNativeInputInfo
}

import scala.annotation.tailrec

object InputUtil {

  /**
    * Returns a valid sequence number for the given [[ScriptNumber]]
    * A transaction needs a valid sequence number to spend a OP_CHECKSEQUENCEVERIFY script.
    * See BIP68/112 for more information
    * [[https://github.com/bitcoin/bips/blob/master/bip-0068.mediawiki]]
    * [[https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki]]
    */
  private def solveSequenceForCSV(scriptNum: ScriptNumber): UInt32 =
    if (LockTimeInterpreter.isCSVLockByBlockHeight(scriptNum)) {
      val blocksPassed = scriptNum.toLong & TransactionConstants.sequenceLockTimeMask.toLong
      UInt32(blocksPassed)
    } else {
      val n = scriptNum.toLong
      val sequence = UInt32(
        n & TransactionConstants.sequenceLockTimeMask.toLong)
      //set sequence number to indicate this is relative locktime
      sequence | TransactionConstants.sequenceLockTimeTypeFlag
    }

  /**
    * This helper function calculates the appropriate sequence number for each transaction input.
    * [[CLTVScriptPubKey]] and [[CSVScriptPubKey]]'s need certain sequence numbers on the inputs
    * to make them spendable.
    * See BIP68/112 and BIP65 for more info
    */
  def calcSequenceForInputs(
      utxos: Seq[InputSigningInfo[InputInfo]],
      isRBFEnabled: Boolean): Seq[TransactionInput] = {
    @tailrec
    def loop(
        remaining: Seq[InputSigningInfo[InputInfo]],
        accum: Seq[TransactionInput]): Seq[TransactionInput] =
      remaining match {
        case Nil => accum.reverse
        case spendingInfo +: newRemaining =>
          spendingInfo.inputInfo match {
            case lockTime: LockTimeInputInfo =>
              val sequence = lockTime.scriptPubKey match {
                case csv: CSVScriptPubKey => solveSequenceForCSV(csv.locktime)
                case _: CLTVScriptPubKey  => UInt32.zero
              }
              val input = TransactionInput(lockTime.outPoint,
                                           EmptyScriptSignature,
                                           sequence)
              loop(newRemaining, input +: accum)
            case p2pkWithTimeout: P2PKWithTimeoutInputInfo =>
              if (p2pkWithTimeout.isBeforeTimeout) {
                val sequence =
                  if (isRBFEnabled) UInt32.zero
                  else TransactionConstants.sequence
                val input =
                  TransactionInput(spendingInfo.outPoint,
                                   EmptyScriptSignature,
                                   sequence)
                loop(newRemaining, input +: accum)
              } else {
                val sequence = solveSequenceForCSV(
                  p2pkWithTimeout.scriptPubKey.lockTime)
                val input = TransactionInput(p2pkWithTimeout.outPoint,
                                             EmptyScriptSignature,
                                             sequence)
                loop(newRemaining, input +: accum)
              }
            case p2sh: P2SHInputInfo =>
              val nestedSpendingInfo =
                p2sh.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, accum)
            case p2wsh: P2WSHV0InputInfo =>
              val nestedSpendingInfo =
                p2wsh.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, accum)
            case conditional: ConditionalInputInfo =>
              val nestedSpendingInfo =
                conditional.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, accum)
            case _: P2WPKHV0InputInfo | _: UnassignedSegwitNativeInputInfo |
                _: P2PKInputInfo | _: P2PKHInputInfo |
                _: MultiSignatureInputInfo | _: EmptyInputInfo =>
              //none of these script types affect the sequence number of a tx
              //the sequence only needs to be adjustd if we have replace by fee (RBF) enabled
              //see BIP125 for more information
              val sequence =
                if (isRBFEnabled) UInt32.zero else TransactionConstants.sequence
              val input =
                TransactionInput(spendingInfo.outPoint,
                                 EmptyScriptSignature,
                                 sequence)
              loop(newRemaining, input +: accum)
          }
      }

    loop(utxos, Nil)
  }
}
