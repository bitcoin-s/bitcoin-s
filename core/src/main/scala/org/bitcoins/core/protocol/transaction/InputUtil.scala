package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  CSVScriptPubKey,
  EmptyScriptSignature
}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.core.wallet.utxo._

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
      val blocksPassed =
        scriptNum.toLong & TransactionConstants.sequenceLockTimeMask.toLong
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
  def calcSequenceForInputInfos(
      utxos: Seq[InputInfo],
      defaultSequence: UInt32 = Policy.sequence): Seq[TransactionInput] = {
    @tailrec
    def loop(
        remaining: Seq[InputInfo],
        accum: Seq[TransactionInput]): Seq[TransactionInput] =
      remaining match {
        case Nil => accum.reverse
        case spendingInfo +: newRemaining =>
          spendingInfo match {
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
                val input =
                  TransactionInput(spendingInfo.outPoint,
                                   EmptyScriptSignature,
                                   defaultSequence)
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
              loop(p2sh.nestedInputInfo +: newRemaining, accum)
            case p2wsh: P2WSHV0InputInfo =>
              loop(p2wsh.nestedInputInfo +: newRemaining, accum)
            case conditional: ConditionalInputInfo =>
              loop(conditional.nestedInputInfo +: newRemaining, accum)
            case _: P2WPKHV0InputInfo | _: UnassignedSegwitNativeInputInfo |
                _: P2PKInputInfo | _: P2PKHInputInfo |
                _: MultiSignatureInputInfo | _: EmptyInputInfo =>
              //none of these script types affect the sequence number of a tx so the defaultSequence is used
              val input =
                TransactionInput(spendingInfo.outPoint,
                                 EmptyScriptSignature,
                                 defaultSequence)
              loop(newRemaining, input +: accum)
          }
      }

    loop(utxos, Nil)
  }

  /**
    * This helper function calculates the appropriate sequence number for each transaction input.
    * [[CLTVScriptPubKey]] and [[CSVScriptPubKey]]'s need certain sequence numbers on the inputs
    * to make them spendable.
    * See BIP68/112 and BIP65 for more info
    */
  def calcSequenceForInputs(
      utxos: Seq[InputSigningInfo[InputInfo]],
      defaultSequence: UInt32 = Policy.sequence): Seq[TransactionInput] = {
    calcSequenceForInputInfos(utxos.map(_.inputInfo), defaultSequence)
  }
}
