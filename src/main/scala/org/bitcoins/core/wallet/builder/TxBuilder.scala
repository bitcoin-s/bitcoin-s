package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.{ECPrivateKey, TxSigComponent}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.signer._

import scala.annotation.tailrec
import scala.util.Try

/** High level class to create a signed transaction that spends a set of
  * unspent transaction outputs.
  */
sealed abstract class TxBuilder {
  private val logger = BitcoinSLogger.logger
  type OutputInfo = (TransactionOutPoint, TransactionOutput, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])
  type OutPointMap = Map[TransactionOutPoint, (TransactionOutput, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])]
  def destinations: Seq[TransactionOutput]

  def destinationSPKs = destinations.map(_.scriptPubKey)

  def destinationAmounts = destinations.map(_.value)

  def totalDestinationAmount = destinationAmounts.fold(CurrencyUnits.zero)(_ + _)

  def creditingTxs: Seq[Transaction]

  /** The unspent transaction outputs we are spending in the new transaction we are building */
  private val creditingUtxos: OutPointMap  = {
    outPointsSpendingInfo.map { case (o,(keys, redeemScriptOpt, scriptWitOpt)) =>
      //this must exist because of our invariant in TransactionBuilderImpl()
      val tx = creditingTxs.find(tx => tx.txId == o.txId).get
      (o,(tx.outputs(o.vout.toInt), keys, redeemScriptOpt, scriptWitOpt))
    }
  }

  /** The list of [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]]s we are attempting to spend
    * and the keys that are needed to sign the utxo we are spending.
    */
  def outPointsSpendingInfo: Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])]

  def privKeys: Seq[ECPrivateKey] = outPointsSpendingInfo.values.flatMap(_._1).toSeq

  def outPoints: Seq[TransactionOutPoint] = outPointsSpendingInfo.keys.toSeq

  def redeemScriptOpt: Seq[Option[ScriptPubKey]] = outPointsSpendingInfo.values.map(_._2).toSeq

  /** Signs the given transaction and then returns a signed tx
    * Checks the given invariants when the signing process is done
    * An example of some invariants is that the fee on the signed transaction below a certain amount,
    * or that RBF is enabled on the signed transaction.
    * */
  def sign(invariants: Transaction => Boolean): Either[Transaction, TxBuilderError] = {
    @tailrec
    def loop(remaining: List[OutputInfo],
             txInProgress: Transaction): Either[Transaction,TxBuilderError] = remaining match {
      case Nil => Left(txInProgress)
      case info :: t =>
        val partiallySigned = sign(info, txInProgress, HashType.sigHashAll)
        partiallySigned match {
          case Left(tx) => loop(t,tx)
          case Right(err) => Right(err)
        }
    }
    val utxos: List[OutputInfo] = creditingUtxos.map { c : (TransactionOutPoint, (TransactionOutput, Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])) =>
      (c._1, c._2._1, c._2._2, c._2._3, c._2._4)
    }.toList
    val outpoints = utxos.map(_._1)
    logger.warn("outpoints before: " + outpoints)
    val scriptWitOpt = utxos.map(_._5)
    val unsignedTxWit = TransactionWitness.fromWitOpt(scriptWitOpt)
    val tc = TransactionConstants
    val lockTime = calcLockTime(utxos)
    val inputs = calcSequenceForInputs(utxos)
    require(inputs.map(_.previousOutput) == outpoints)
    logger.info(s"inputs: $inputs")
    val unsigned = unsignedTxWit match {
      case EmptyWitness => BaseTransaction(tc.validLockVersion,inputs,destinations,lockTime)
      case wit: TransactionWitness => WitnessTransaction(tc.validLockVersion,inputs,destinations,lockTime,wit)
    }
    val signedTx = loop(utxos, unsigned)
    signedTx match {
      case l: Left[Transaction,TxBuilderError] =>
        if (!invariants(l.a)) {
          Right(TxBuilderError.FailedUserInvariants)
        } else {
          l
        }
      case r: Right[Transaction, TxBuilderError] => r
    }
  }

  /** Creates a newly signed input and adds it to the given tx */
  private def sign(info: OutputInfo, unsignedTx: Transaction, hashType: HashType): Either[Transaction, TxBuilderError] = {
    val outpoint = info._1
    val output = info._2
    val keys = info._3
    val redeemScriptOpt = info._4
    val scriptWitnessOpt = info._5
    val inputIndex = UInt32(unsignedTx.inputs.zipWithIndex.find(_._1.previousOutput == outpoint).get._2)
    val oldInput = unsignedTx.inputs(inputIndex.toInt)
    output.scriptPubKey match {
      case _: P2PKScriptPubKey =>
        P2PKSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
      case _: P2PKHScriptPubKey => P2PKHSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
      case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
      case lock: LockTimeScriptPubKey =>
        lock.nestedScriptPubKey match {
          case _: P2PKScriptPubKey => P2PKSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
          case _: P2PKHScriptPubKey => P2PKHSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
          case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
          case _: P2WPKHWitnessSPKV0 => P2WPKHSigner.sign(keys,output,unsignedTx,inputIndex,hashType).left.map(_.transaction)
          case _: P2SHScriptPubKey => Right(TxBuilderError.NestedP2SHSPK)
          case _: P2WSHWitnessSPKV0 => Right(TxBuilderError.NestedP2WSHSPK)
          case _: CSVScriptPubKey | _: CLTVScriptPubKey =>
            //TODO: Comeback to this later and see if we should have signer for nested locktime spks
            Right(TxBuilderError.NoSigner)
          case _: NonStandardScriptPubKey | _: WitnessCommitment
               | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey => Right(TxBuilderError.NoSigner)
        }
      case _: P2SHScriptPubKey =>
        redeemScriptOpt match {
          case Some(redeemScript) =>
            val input = TransactionInput(outpoint,EmptyScriptSignature,oldInput.sequence)
            val updatedTx = unsignedTx match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,unsignedTx.inputs.updated(inputIndex.toInt,input),btx.outputs,btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,unsignedTx.inputs.updated(inputIndex.toInt,input),wtx.outputs,wtx.lockTime,wtx.witness)
            }
            val updatedOutput = TransactionOutput(output.value,redeemScript)
            val signedTxEither: Either[Transaction, TxBuilderError] = sign((outpoint,updatedOutput,keys,None,
              scriptWitnessOpt),updatedTx,hashType)
            signedTxEither.left.map { signedTx =>
              val i = signedTx.inputs(inputIndex.toInt)
              val p2sh = P2SHScriptSignature(i.scriptSignature,redeemScript)
              val signedInput = TransactionInput(i.previousOutput,p2sh,i.sequence)
              val signedInputs = signedTx.inputs.updated(inputIndex.toInt,signedInput)
              signedTx match {
                case btx: BaseTransaction =>
                  BaseTransaction(btx.version,signedInputs,btx.outputs,btx.lockTime)
                case wtx: WitnessTransaction =>
                  WitnessTransaction(wtx.version,signedInputs,wtx.outputs,wtx.lockTime,wtx.witness)
              }
            }
          case None => Right(TxBuilderError.NoRedeemScript)
        }
      case _: WitnessScriptPubKeyV0 =>
        //if we don't have a WitnessTransaction we need to convert our unsignedTx to a WitnessTransaction
        val unsignedWTx: WitnessTransaction = unsignedTx match {
          case btx: BaseTransaction => WitnessTransaction(btx.version, btx.inputs, btx.outputs,btx.lockTime, EmptyWitness)
          case wtx: WitnessTransaction => wtx
        }
        val result: Either[TxSigComponent, TxBuilderError] = scriptWitnessOpt match {
          case Some(scriptWit) =>
            scriptWit match {
              case _: P2WPKHWitnessV0 =>
                if (keys.size != 1) {
                  Right(TxBuilderError.TooManyKeys)
                } else {
                  P2WPKHSigner.sign(keys, output, unsignedWTx, inputIndex, hashType)
                }
              case p2wshScriptWit: P2WSHWitnessV0 =>
                val redeemScript = p2wshScriptWit.redeemScript
                redeemScript match {
                  case _: P2PKScriptPubKey => P2PKSigner.sign(keys,output,unsignedWTx,inputIndex,hashType)
                  case _: P2PKHScriptPubKey => P2PKHSigner.sign(keys,output,unsignedWTx,inputIndex,hashType)
                  case _: MultiSignatureScriptPubKey  => MultiSigSigner.sign(keys,output,unsignedWTx,inputIndex,hashType)
                  case _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 => Right(TxBuilderError.NestedWitnessSPK)
                  case _: P2SHScriptPubKey => Right(TxBuilderError.NestedP2SHSPK)
                  case lock: LockTimeScriptPubKey =>
                    lock.nestedScriptPubKey match {
                      case _: P2PKScriptPubKey => P2PKSigner.sign(keys,output,unsignedTx,inputIndex,hashType)
                      case _: P2PKHScriptPubKey => P2PKHSigner.sign(keys,output,unsignedTx,inputIndex,hashType)
                      case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(keys,output,unsignedTx,inputIndex,hashType)
                      case _: P2WPKHWitnessSPKV0 => P2WPKHSigner.sign(keys,output,unsignedTx,inputIndex,hashType)
                      case _: P2SHScriptPubKey => Right(TxBuilderError.NestedP2SHSPK)
                      case _: P2WSHWitnessSPKV0 => Right(TxBuilderError.NestedP2WSHSPK)
                      case _: CSVScriptPubKey | _: CLTVScriptPubKey =>
                        //TODO: Comeback to this later and see if we should have signer for nested locktime spks
                        Right(TxBuilderError.NoSigner)
                      case _: NonStandardScriptPubKey | _: WitnessCommitment
                           | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey => Right(TxBuilderError.NoSigner)
                    }
                  case (_: NonStandardScriptPubKey | _: WitnessCommitment | EmptyScriptPubKey
                        | _: UnassignedWitnessScriptPubKey) =>
                    Right(TxBuilderError.NoSigner)
                }
              case EmptyScriptWitness => Right(TxBuilderError.NoWitness)
            }
          case None => Right(TxBuilderError.NoWitness)
        }
        result.left.map(_.transaction)

      case _: NonStandardScriptPubKey | _: WitnessCommitment
           | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey => Right(TxBuilderError.NoSigner)
    }
  }

/*  private def matchSigner(spk: ScriptPubKey): Option[Signer] = spk match {
    case _: P2PKScriptPubKey => Some(P2PKSigner)
    case _: P2PKHScriptPubKey => Some(P2PKHSigner)
    case _: MultiSignatureScriptPubKey => Some(MultiSigSigner)
    case _: P2SHScriptPubKey => None
    case _: CSVScriptPubKey | _: CLTVScriptPubKey => None
    case _: P2WPKHWitnessSPKV0 => Some(P2WPKHSigner)
    case _: WitnessCommitment | EmptyScriptPubKey
         | _: UnassignedWitnessScriptPubKey | _: NonStandardScriptPubKey => None
  }*/

  /** Returns a valid sequence number for the given [[ScriptNumber]]
    * A transaction needs a valid sequence number to spend a OP_CHECKSEQUENCEVERIFY script.
    * See BIP68/112 for more information
    * [[https://github.com/bitcoin/bips/blob/master/bip-0068.mediawiki]]
    * [[https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki]]
    * */
  private def solveSequenceForCSV(scriptNum: ScriptNumber): UInt32 = LockTimeInterpreter.isCSVLockByBlockHeight(scriptNum) match {
    case true =>
      //means that we need to have had scriptNum blocks bassed since this tx was included a block to be able to spend this output
      val blocksPassed = scriptNum.toLong & TransactionConstants.sequenceLockTimeMask.toLong
      val sequence = UInt32(blocksPassed)
      sequence
    case false =>
      //means that we need to have had 512 * n seconds passed since the tx was included in a block passed
      val n = scriptNum.toLong
      val sequence = UInt32(n & TransactionConstants.sequenceLockTimeMask.toLong)
      //set sequence number to indicate this is relative locktime
      sequence | TransactionConstants.sequenceLockTimeTypeFlag
  }

  /** Returns a valid locktime for a transaction that is a valid for the given [[ScriptNumber]]
    *
    */
  private def solveLockTimeForCLTV(scriptNum: ScriptNumber): UInt32 = UInt32(scriptNum.toLong)

  private def calcLockTime(utxos: Seq[OutputInfo]): UInt32 = {
    @tailrec
    def loop(remaining: Seq[OutputInfo], currentLockTime: UInt32): UInt32 = remaining match {
      case Nil => currentLockTime
      case (outpoint,output,keys,redeemScriptOpt,scriptWitOpt) :: t => output.scriptPubKey match {
        case cltv: CLTVScriptPubKey =>
          val l = UInt32(cltv.locktime.toLong)
          if (currentLockTime < l) loop(t,l)
          else loop(t,currentLockTime)
        case _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 =>
          if (redeemScriptOpt.isDefined) {
            //recursively call with redeem script as output script
            val o = TransactionOutput(output.value,redeemScriptOpt.get)
            val i = (outpoint,o, keys, None,scriptWitOpt)
            loop(i +: t, currentLockTime)
          } else if (scriptWitOpt.isDefined) {
            scriptWitOpt.get match {
              case EmptyScriptWitness => loop(t,currentLockTime)
              case _: P2WPKHWitnessV0 => loop(t,currentLockTime)
              case p2wsh: P2WSHWitnessV0 =>
                //recursively call with the witness redeem script as the script
                val o = TransactionOutput(output.value, p2wsh.redeemScript)
                val i = (outpoint,o,keys,redeemScriptOpt,None)
                loop(i +: t, currentLockTime)
            }
          } else {
            loop(t,currentLockTime)
          }
        case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
             | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey | _: WitnessCommitment
             | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey | _: CSVScriptPubKey  =>
          loop(t,currentLockTime)
      }
    }
    loop(utxos,TransactionConstants.lockTime)
  }

  private def calcSequenceForInputs(utxos: Seq[OutputInfo]): Seq[TransactionInput] = {
    @tailrec
    def loop(remaining: Seq[OutputInfo], accum: Seq[TransactionInput]): Seq[TransactionInput] = remaining match {
      case Nil => accum.reverse
      case (outpoint,output,keys,redeemScriptOpt,scriptWitOpt) :: t =>
        output.scriptPubKey match {
          case csv: CSVScriptPubKey =>
            val sequence = solveSequenceForCSV(csv.locktime)
            val i = TransactionInput(outpoint,EmptyScriptSignature,sequence)
            loop(t,i +: accum)
          case _: CLTVScriptPubKey =>
            val sequence = UInt32.zero
            val i = TransactionInput(outpoint,EmptyScriptSignature,sequence)
            loop(t,i +: accum)
          case _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 =>
            if (redeemScriptOpt.isDefined) {
              //recursively call with the redeem script in the output
              val o = TransactionOutput(output.value,redeemScriptOpt.get)
              val i = (outpoint,o,keys,None,scriptWitOpt)
              loop(i +: t, accum)
            } else if (scriptWitOpt.isDefined) {
              scriptWitOpt.get match {
                case EmptyScriptWitness => loop(t,accum)
                case _: P2WPKHWitnessV0 => loop(t,accum)
                case p2wsh: P2WSHWitnessV0 =>
                  val o = TransactionOutput(output.value,p2wsh.redeemScript)
                  val i = (outpoint,o,keys,redeemScriptOpt,None)
                  loop(i +: t, accum)
              }
            } else loop(t,accum)
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey
               | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey | _: WitnessCommitment
               | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey  =>
            val input = TransactionInput(outpoint,EmptyScriptSignature,TransactionConstants.sequence)
            loop(t, input +: accum)

        }
    }
    val inputs = loop(utxos,Nil)
    val outpoints = inputs.map(_.previousOutput)
    logger.warn("outpoints: " + outpoints)
    inputs
  }
}


object TxBuilder {
  private case class TransactionBuilderImpl(destinations: Seq[TransactionOutput],
                                            creditingTxs: Seq[Transaction],
                                            outPointsSpendingInfo: Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])]) extends TxBuilder {
    require(outPoints.exists(o => creditingTxs.exists(_.txId == o.txId)))
  }

  def apply(destinations: Seq[TransactionOutput], creditingTxs: Seq[Transaction],
            outPointsSpendingInfo: Map[TransactionOutPoint, (Seq[ECPrivateKey], Option[ScriptPubKey], Option[ScriptWitness])],
            version: UInt32, locktime: UInt32): Try[TxBuilder] = {
    Try(TransactionBuilderImpl(destinations,creditingTxs,outPointsSpendingInfo))
  }
}
