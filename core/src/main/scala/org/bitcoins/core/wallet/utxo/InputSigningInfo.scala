package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  CSVScriptPubKey,
  EmptyScriptSignature
}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionConstants,
  TransactionInput,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.crypto.Sign

import scala.annotation.tailrec

/** Stores the information required to generate a signature (ECSignatureParams)
  * or to generate a script signature (ScriptSignatureParams) for a given satisfaction
  * condition on a UTXO.
  */
sealed trait InputSigningInfo[+InputType <: InputInfo] {
  def inputInfo: InputType
  def hashType: HashType
  def signers: Vector[Sign]

  private val keysToSignFor = inputInfo.pubKeys
  require(signers.map(_.publicKey).forall(keysToSignFor.contains),
          s"Cannot have signers that do not sign for one of $keysToSignFor")

  def outputReference: OutputReference = inputInfo.outputReference
  def amount: CurrencyUnit = inputInfo.amount
  def output: TransactionOutput = inputInfo.output
  def outPoint: TransactionOutPoint = inputInfo.outPoint
  def conditionalPath: ConditionalPath = inputInfo.conditionalPath
}

object InputSigningInfo {

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
      val sequence = UInt32(blocksPassed)
      sequence
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
                val input = TransactionInput(p2pkWithTimeout.outPoint,
                                             EmptyScriptSignature,
                                             UInt32.zero)
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

/** Stores the information needed to generate a ScriptSignature for a specific
  * spending condition on a UTXO.
  */
case class ScriptSignatureParams[+InputType <: InputInfo](
    inputInfo: InputType,
    signers: Vector[Sign],
    hashType: HashType)
    extends InputSigningInfo[InputType] {

  def signer: Sign = {
    require(
      signers.length == 1,
      "This method is for spending infos with a single signer, if you mean signers.head be explicit")

    signers.head
  }

  def toSingle(index: Int): ECSignatureParams[InputType] = {
    ECSignatureParams(inputInfo, signers(index), hashType)
  }

  def toSingles: Vector[ECSignatureParams[InputType]] = {
    signers.map { signer =>
      ECSignatureParams(inputInfo, signer, hashType)
    }
  }

  def mapInfo[T <: InputInfo](
      func: InputType => T): ScriptSignatureParams[T] = {
    this.copy(inputInfo = func(this.inputInfo))
  }
}

object ScriptSignatureParams {

  def apply[InputType <: InputInfo](
      inputInfo: InputType,
      signer: Sign,
      hashType: HashType): ScriptSignatureParams[InputType] =
    ScriptSignatureParams(inputInfo, Vector(signer), hashType)
}

/** Stores the information needed to generate an ECDigitalSignature for
  * a use in spending a UTXO.
  */
case class ECSignatureParams[+InputType <: InputInfo](
    inputInfo: InputType,
    signer: Sign,
    hashType: HashType)
    extends InputSigningInfo[InputType] {
  override def signers: Vector[Sign] = Vector(signer)

  def mapInfo[T <: InputInfo](func: InputType => T): ECSignatureParams[T] = {
    this.copy(inputInfo = func(this.inputInfo))
  }
}
