package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.{ BitcoinNetwork, NetworkParameters }
import org.bitcoins.core.currency.{ CurrencyUnit, CurrencyUnits, Satoshis }
import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder.TxBuilder.{ UTXOMap, UTXOTuple }
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer._

import scala.annotation.tailrec

/**
 * High level class to create a signed transaction that spends a set of
 * unspent transaction outputs.
 *
 * The most important method in this class is the 'sign' method. This will start the signing procedure for a
 * transaction and then return either a signed [[Transaction]] or a [[TxBuilderError]]
 *
 * For usage examples see TxBuilderSpec
 */
sealed abstract class TxBuilder {
  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants

  /** The outputs which we are spending bitcoins to */
  def destinations: Seq[TransactionOutput]

  /** The [[ScriptPubKey]]'s we are spending bitcoins to */
  def destinationSPKs: Seq[ScriptPubKey] = destinations.map(_.scriptPubKey)

  /** A sequence of the amounts we are spending in this transaction */
  def destinationAmounts: Seq[CurrencyUnit] = destinations.map(_.value)

  /** The spent amount of bitcoins we are sending in the transaction, this does NOT include the fee */
  def destinationAmount: CurrencyUnit = destinationAmounts.fold(CurrencyUnits.zero)(_ + _)

  /** The total amount of satoshis that are able to be spent by this transaction */
  def creditingAmount: CurrencyUnit = utxoMap.values.map(_._1.value).fold(CurrencyUnits.zero)(_ + _)

  /** The largest possible fee in this transaction could pay */
  def largestFee: CurrencyUnit = creditingAmount - destinationAmount

  /**
   * The transactions which contain the outputs we are spending. We need various bits of information from
   * these crediting transactions, like there txid, the output amount, and obviously the ouptut [[ScriptPubKey]]
   */
  def creditingTxs: Seq[Transaction]

  /**
   * The list of [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]]s we are attempting to spend
   * and the signers, redeem scripts, and script witnesses that might be needed to spend this outpoint.
   * This information is dependent on what the [[ScriptPubKey]] type is we are spending. For isntance, if we are spending a
   * regular [[P2PKHScriptPubKey]], we do not need a redeem script or script witness.
   *
   * If we are spending a [[P2WPKHWitnessSPKV0]] we do not need a redeem script, but we need a [[ScriptWitness]]
   */
  def utxoMap: TxBuilder.UTXOMap

  /** This represents the rate, in [[FeeUnit]], we should pay for this transaction */
  def feeRate: FeeUnit

  /**
   * This is where all the money that is NOT sent to destination outputs is spent too.
   * If we don't specify a change output, a large miner fee may be paid as more than likely
   * the difference between [[creditingAmount]] and [[destinationAmount]] is not a market rate miner fee
   */
  def changeSPK: ScriptPubKey

  /**
   * The network that this [[org.bitcoins.core.wallet.builder.TxBuilder]] is signing a transaction for.
   * An example could be [[org.bitcoins.core.config.MainNet]]
   */
  def network: NetworkParameters

  /** The outpoints that we are using in this transaction */
  def outPoints: Seq[TransactionOutPoint] = utxoMap.keys.toSeq

  /** The redeem scripts that are needed in this transaction */
  def redeemScriptOpt: Seq[Option[ScriptPubKey]] = utxoMap.values.map(_._3).toSeq

  /** The script witnesses that are needed in this transaction */
  def scriptWitOpt: Seq[Option[ScriptWitness]] = utxoMap.values.map(_._4).toSeq

  def sign: Either[Transaction, TxBuilderError]

  /** Overloaded version of sign that skips passing a user invariant */
  def sign(isRBFEnabled: Boolean): Either[Transaction, TxBuilderError]

  /**
   * Signs the given transaction and then returns a signed tx that spends
   * all of the given outputs.
   * Checks the given invariants when the signing process is done
   * An example of some invariants is that the fee on the signed transaction below a certain amount,
   * or that RBF is enabled on the signed transaction.
   *
   * @param invariants - invariants that should hold true when we are done signing the transaction
   * @param isRBFEnabled - if we should enable replace-by-fee on this transaction, see BIP125
   * @return the signed transaction, or a [[TxBuilderError]] indicating what went wrong when signing the tx
   */
  def sign(invariants: (UTXOMap, Transaction) => Boolean, isRBFEnabled: Boolean = false): Either[Transaction, TxBuilderError]

}

/**
 * The [[org.bitcoins.core.wallet.builder.TxBuilder]] for the
 * bitcoin network(s) [[org.bitcoins.core.config.BitcoinNetwork]]
 */
sealed abstract class BitcoinTxBuilder extends TxBuilder {
  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants

  override def network: BitcoinNetwork

  override def sign: Either[Transaction, TxBuilderError] = sign(Policy.isRBFEnabled)

  /** Overloaded version of sign that skips passing a user invariant */
  override def sign(isRBFEnabled: Boolean): Either[Transaction, TxBuilderError] = {
    //trivially true function
    val f = (_: UTXOMap, _: Transaction) => true
    sign(f, isRBFEnabled)
  }

  /**
   * Signs the given transaction and then returns a signed tx that spends
   * all of the given outputs.
   * Checks the given invariants when the signing process is done
   * An example of some invariants is that the fee on the signed transaction below a certain amount,
   * or that RBF is enabled on the signed transaction.
   *
   * @param invariants - invariants that should hold true when we are done signing the transaction
   * @param isRBFEnabled - if we should enable replace-by-fee on this transaction, see BIP125
   * @return the signed transaction, or a [[TxBuilderError]] indicating what went wrong when signing the tx
   */
  override def sign(invariants: (UTXOMap, Transaction) => Boolean, isRBFEnabled: Boolean = false): Either[Transaction, TxBuilderError] = {
    @tailrec
    def loop(
      remaining:    List[TxBuilder.UTXOTuple],
      txInProgress: Transaction
    ): Either[Transaction, TxBuilderError] = remaining match {
      case Nil => Left(txInProgress)
      case info :: t =>
        val partiallySigned = sign(info, txInProgress)
        partiallySigned match {
          case Left(tx)   => loop(t, tx)
          case Right(err) => Right(err)
        }
    }
    val utxos: List[TxBuilder.UTXOTuple] = utxoMap.map { c =>
      (c._1, c._2._1, c._2._2, c._2._3, c._2._4, c._2._5)
    }.toList
    val unsignedTxWit = TransactionWitness.fromWitOpt(scriptWitOpt)
    val lockTime = calcLockTime(utxos)
    val inputs = calcSequenceForInputs(utxos, isRBFEnabled)
    val changeOutput = TransactionOutput(CurrencyUnits.zero, changeSPK)
    val unsignedTxNoFee = lockTime.left.map { l =>
      unsignedTxWit match {
        case EmptyWitness            => BaseTransaction(tc.validLockVersion, inputs, destinations ++ Seq(changeOutput), l)
        case wit: TransactionWitness => WitnessTransaction(tc.validLockVersion, inputs, destinations ++ Seq(changeOutput), l, wit)
      }
    }
    //NOTE: This signed version of the tx does NOT pay a fee, we are going to use this version to estimate the fee
    //and then deduct that amount of satoshis from the changeOutput, and then resign the tx.
    val signedTxNoFee = unsignedTxNoFee.left.flatMap(utxnf => loop(utxos, utxnf))
    signedTxNoFee match {
      case l: Left[Transaction, TxBuilderError] =>
        val fee = feeRate.calc(l.a)
        val newChangeOutput = TransactionOutput(creditingAmount - destinationAmount - fee, changeSPK)
        //if the change output is below the dust threshold after calculating the fee, don't add it
        //to the tx
        val newOutputs = if (newChangeOutput.value <= Policy.dustThreshold) {
          logger.debug("removing change output as value is below the dustThreshold")
          destinations
        } else {
          destinations ++ Seq(newChangeOutput)
        }
        val unsignedTx = unsignedTxWit match {
          case EmptyWitness            => BaseTransaction(l.a.version, inputs, newOutputs, l.a.lockTime)
          case wit: TransactionWitness => WitnessTransaction(l.a.version, inputs, newOutputs, l.a.lockTime, wit)
        }
        //re-sign the tx with the appropriate change / fee
        val signedTx = loop(utxos, unsignedTx)
        signedTx.left.flatMap { tx =>
          if (invariants(utxoMap, tx)) {
            //final sanity checks
            val err = BitcoinTxBuilder.sanityChecks(this, tx)
            if (err.isDefined) Right(err.get)
            else Left(tx)
          } else Right(TxBuilderError.FailedUserInvariants)
        }
      case r: Right[Transaction, TxBuilderError] => r
    }
  }

  /**
   * This function creates a newly signed input, and then adds it to the unsigned transaction
   * @param utxo - the information needed to validly spend the given output
   * @param unsignedTx - the transaction that we are spending this output in
   * @return either the transaction with the signed input added, or a [[TxBuilderError]]
   */
  private def sign(utxo: TxBuilder.UTXOTuple, unsignedTx: Transaction): Either[Transaction, TxBuilderError] = {
    val outpoint = utxo._1
    val output = utxo._2
    val signers = utxo._3
    val redeemScriptOpt = utxo._4
    val scriptWitnessOpt = utxo._5
    val hashType = utxo._6
    val idx = unsignedTx.inputs.zipWithIndex.find(_._1.previousOutput == outpoint)
    if (idx.isEmpty) {
      Right(TxBuilderError.MissingOutPoint)
    } else {
      val inputIndex = UInt32(idx.get._2)
      val oldInput = unsignedTx.inputs(inputIndex.toInt)
      output.scriptPubKey match {
        case _: P2PKScriptPubKey =>
          P2PKSigner.sign(signers, output, unsignedTx, inputIndex, hashType).left.map(_.transaction)
        case _: P2PKHScriptPubKey          => P2PKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType).left.map(_.transaction)
        case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(signers, output, unsignedTx, inputIndex, hashType).left.map(_.transaction)
        case lock: LockTimeScriptPubKey =>
          lock.nestedScriptPubKey match {
            case _: P2PKScriptPubKey                          => P2PKSigner.sign(signers, output, unsignedTx, inputIndex, hashType).left.map(_.transaction)
            case _: P2PKHScriptPubKey                         => P2PKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType).left.map(_.transaction)
            case _: MultiSignatureScriptPubKey                => MultiSigSigner.sign(signers, output, unsignedTx, inputIndex, hashType).left.map(_.transaction)
            case _: P2SHScriptPubKey                          => Right(TxBuilderError.NestedP2SHSPK)
            case _: P2WSHWitnessSPKV0 | _: P2WPKHWitnessSPKV0 => Right(TxBuilderError.NestedWitnessSPK)
            case _: CSVScriptPubKey | _: CLTVScriptPubKey
              | _: NonStandardScriptPubKey | _: WitnessCommitment | _: EscrowTimeoutScriptPubKey
              | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey => Right(TxBuilderError.NoSigner)
          }
        case p2sh: P2SHScriptPubKey =>
          redeemScriptOpt match {
            case Some(redeemScript) =>
              if (p2sh != P2SHScriptPubKey(redeemScript)) {
                Right(TxBuilderError.WrongRedeemScript)
              } else {
                val input = TransactionInput(outpoint, EmptyScriptSignature, oldInput.sequence)
                val updatedTx = unsignedTx match {
                  case btx: BaseTransaction =>
                    BaseTransaction(btx.version, unsignedTx.inputs.updated(inputIndex.toInt, input), btx.outputs, btx.lockTime)
                  case wtx: WitnessTransaction =>
                    WitnessTransaction(wtx.version, unsignedTx.inputs.updated(inputIndex.toInt, input), wtx.outputs, wtx.lockTime, wtx.witness)
                }
                val updatedOutput = TransactionOutput(output.value, redeemScript)
                val signedTxEither: Either[Transaction, TxBuilderError] = sign((outpoint, updatedOutput, signers, None,
                  scriptWitnessOpt, hashType), updatedTx)
                signedTxEither.left.map { signedTx =>
                  val i = signedTx.inputs(inputIndex.toInt)
                  val p2sh = P2SHScriptSignature(i.scriptSignature, redeemScript)
                  val signedInput = TransactionInput(i.previousOutput, p2sh, i.sequence)
                  val signedInputs = signedTx.inputs.updated(inputIndex.toInt, signedInput)
                  signedTx match {
                    case btx: BaseTransaction =>
                      BaseTransaction(btx.version, signedInputs, btx.outputs, btx.lockTime)
                    case wtx: WitnessTransaction =>
                      WitnessTransaction(wtx.version, signedInputs, wtx.outputs, wtx.lockTime, wtx.witness)
                  }
                }
              }
            case None => Right(TxBuilderError.NoRedeemScript)
          }

        case _: P2WPKHWitnessSPKV0 =>
          //if we don't have a WitnessTransaction we need to convert our unsignedTx to a WitnessTransaction
          val unsignedWTx: WitnessTransaction = unsignedTx match {
            case btx: BaseTransaction    => WitnessTransaction(btx.version, btx.inputs, btx.outputs, btx.lockTime, EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val result = P2WPKHSigner.sign(signers, output, unsignedWTx, inputIndex, hashType)
          result.left.map(_.transaction)
        case p2wshSPK: P2WSHWitnessSPKV0 =>
          //if we don't have a WitnessTransaction we need to convert our unsignedTx to a WitnessTransaction
          val unsignedWTx: WitnessTransaction = unsignedTx match {
            case btx: BaseTransaction    => WitnessTransaction(btx.version, btx.inputs, btx.outputs, btx.lockTime, EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val p2wshScriptWit = scriptWitnessOpt match {
            case Some(wit) =>
              wit match {
                case EmptyScriptWitness | _: P2WPKHWitnessV0 => Right(TxBuilderError.WrongWitness)
                case x: P2WSHWitnessV0                       => Left(x)
              }
            case None => Right(TxBuilderError.NoWitness)
          }
          val redeemScriptEither = p2wshScriptWit.left.map(_.redeemScript)
          val result = redeemScriptEither.left.flatMap { redeemScript =>
            if (P2WSHWitnessSPKV0(redeemScript) != p2wshSPK) {
              Right(TxBuilderError.WrongWitness)
            } else {
              redeemScript match {
                case _: P2PKScriptPubKey                          => P2PKSigner.sign(signers, output, unsignedWTx, inputIndex, hashType)
                case _: P2PKHScriptPubKey                         => P2PKHSigner.sign(signers, output, unsignedWTx, inputIndex, hashType)
                case _: MultiSignatureScriptPubKey                => MultiSigSigner.sign(signers, output, unsignedWTx, inputIndex, hashType)
                case _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 => Right(TxBuilderError.NestedWitnessSPK)
                case _: P2SHScriptPubKey                          => Right(TxBuilderError.NestedP2SHSPK)
                case lock: LockTimeScriptPubKey =>
                  lock.nestedScriptPubKey match {
                    case _: P2PKScriptPubKey           => P2PKSigner.sign(signers, output, unsignedTx, inputIndex, hashType)
                    case _: P2PKHScriptPubKey          => P2PKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType)
                    case _: MultiSignatureScriptPubKey => MultiSigSigner.sign(signers, output, unsignedTx, inputIndex, hashType)
                    case _: P2WPKHWitnessSPKV0         => P2WPKHSigner.sign(signers, output, unsignedTx, inputIndex, hashType)
                    case _: P2SHScriptPubKey           => Right(TxBuilderError.NestedP2SHSPK)
                    case _: P2WSHWitnessSPKV0          => Right(TxBuilderError.NestedWitnessSPK)
                    case _: CSVScriptPubKey | _: CLTVScriptPubKey | _: EscrowTimeoutScriptPubKey
                      | _: NonStandardScriptPubKey | _: WitnessCommitment
                      | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
                      | _: EscrowTimeoutScriptPubKey => Right(TxBuilderError.NoSigner)
                  }
                case _: NonStandardScriptPubKey | _: WitnessCommitment | EmptyScriptPubKey
                  | _: UnassignedWitnessScriptPubKey | _: EscrowTimeoutScriptPubKey =>
                  Right(TxBuilderError.NoSigner)
              }
            }
          }
          result.left.map(_.transaction)
        case _: NonStandardScriptPubKey | _: WitnessCommitment
          | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
          | _: EscrowTimeoutScriptPubKey => Right(TxBuilderError.NoSigner)
      }
    }
  }

  /**
   * Returns a valid sequence number for the given [[ScriptNumber]]
   * A transaction needs a valid sequence number to spend a OP_CHECKSEQUENCEVERIFY script.
   * See BIP68/112 for more information
   * [[https://github.com/bitcoin/bips/blob/master/bip-0068.mediawiki]]
   * [[https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki]]
   */
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

  /**
   * This helper function calculates the appropriate locktime for a transaction.
   * To be able to spend [[CLTVScriptPubKey]]'s you need to have the transaction's
   * locktime set to the same value (or higher) than the output it is spending.
   * See BIP65 for more info
   */
  private def calcLockTime(utxos: Seq[TxBuilder.UTXOTuple]): Either[UInt32, TxBuilderError] = {
    @tailrec
    def loop(remaining: Seq[TxBuilder.UTXOTuple], currentLockTime: UInt32): Either[UInt32, TxBuilderError] = remaining match {
      case Nil => Left(currentLockTime)
      case (outpoint, output, signers, redeemScriptOpt, scriptWitOpt, hashType) :: t => output.scriptPubKey match {
        case cltv: CLTVScriptPubKey =>
          val lockTime: Either[UInt32, TxBuilderError] = if (cltv.locktime.toLong > UInt32.max.toLong || cltv.locktime.toLong < 0) {
            Right(TxBuilderError.IncompatibleLockTimes)
          } else Left(UInt32(cltv.locktime.toLong))
          val result: Either[UInt32, TxBuilderError] = lockTime.left.flatMap { l: UInt32 =>
            if (currentLockTime < l) {
              val lockTimeThreshold = tc.locktimeThreshold
              if (currentLockTime < lockTimeThreshold && l >= lockTimeThreshold) {
                //means that we spend two different locktime types, one of the outputs spends a
                //OP_CLTV script by block height, the other spends one by time stamp
                Right(TxBuilderError.IncompatibleLockTimes)
              } else Left(l)
            } else Left(currentLockTime)
          }
          result match {
            case Left(l)  => loop(t, l)
            case Right(r) => Right(r)
          }
        case _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 =>
          if (redeemScriptOpt.isDefined) {
            //recursively call with redeem script as output script
            val o = TransactionOutput(output.value, redeemScriptOpt.get)
            val i = (outpoint, o, signers, None, scriptWitOpt, hashType)
            loop(i +: t, currentLockTime)
          } else if (scriptWitOpt.isDefined) {
            scriptWitOpt.get match {
              case EmptyScriptWitness => loop(t, currentLockTime)
              case _: P2WPKHWitnessV0 => loop(t, currentLockTime)
              case p2wsh: P2WSHWitnessV0 =>
                //recursively call with the witness redeem script as the script
                val o = TransactionOutput(output.value, p2wsh.redeemScript)
                val i = (outpoint, o, signers, redeemScriptOpt, None, hashType)
                loop(i +: t, currentLockTime)
            }
          } else {
            loop(t, currentLockTime)
          }
        case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
          | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey | _: WitnessCommitment
          | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey | _: CSVScriptPubKey
          | _: EscrowTimeoutScriptPubKey =>
          //non of these scripts affect the locktime of a tx
          loop(t, currentLockTime)
      }
    }
    loop(utxos, TransactionConstants.lockTime)
  }

  /**
   * This helper function calculates the appropriate sequence number for each transaction input.
   * [[CLTVScriptPubKey]] and [[CSVScriptPubKey]]'s need certain sequence numbers on the inputs
   * to make them spendable.
   * See BIP68/112 and BIP65 for more info
   */
  private def calcSequenceForInputs(utxos: Seq[TxBuilder.UTXOTuple], isRBFEnabled: Boolean): Seq[TransactionInput] = {
    @tailrec
    def loop(remaining: Seq[TxBuilder.UTXOTuple], accum: Seq[TransactionInput]): Seq[TransactionInput] = remaining match {
      case Nil => accum.reverse
      case (outpoint, output, signers, redeemScriptOpt, scriptWitOpt, hashType) :: t =>
        output.scriptPubKey match {
          case csv: CSVScriptPubKey =>
            val sequence = solveSequenceForCSV(csv.locktime)
            val i = TransactionInput(outpoint, EmptyScriptSignature, sequence)
            loop(t, i +: accum)
          case _: CLTVScriptPubKey =>
            val sequence = UInt32.zero
            val i = TransactionInput(outpoint, EmptyScriptSignature, sequence)
            loop(t, i +: accum)
          case _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 =>
            if (redeemScriptOpt.isDefined) {
              //recursively call with the redeem script in the output
              val o = TransactionOutput(output.value, redeemScriptOpt.get)
              val i = (outpoint, o, signers, None, scriptWitOpt, hashType)
              loop(i +: t, accum)
            } else if (scriptWitOpt.isDefined) {
              scriptWitOpt.get match {
                case EmptyScriptWitness | _: P2WPKHWitnessV0 => loop(t, accum)
                case p2wsh: P2WSHWitnessV0 =>
                  val o = TransactionOutput(output.value, p2wsh.redeemScript)
                  val i = (outpoint, o, signers, redeemScriptOpt, None, hashType)
                  loop(i +: t, accum)
              }
            } else loop(t, accum)
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey
            | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey | _: WitnessCommitment
            | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey | _: EscrowTimeoutScriptPubKey =>
            //none of these script types affect the sequence number of a tx
            //the sequence only needs to be adjustd if we have replace by fee (RBF) enabled
            //see BIP125 for more information
            val sequence = if (isRBFEnabled) UInt32.zero else TransactionConstants.sequence
            val input = TransactionInput(outpoint, EmptyScriptSignature, sequence)
            loop(t, input +: accum)
        }
    }
    val inputs = loop(utxos, Nil)
    inputs
  }
}

object TxBuilder {
  /** This contains all the information needed to create a valid [[TransactionInput]] that spends this utxo */
  type UTXOTuple = (TransactionOutPoint, TransactionOutput, Seq[Signer.Sign], Option[ScriptPubKey], Option[ScriptWitness], HashType)
  type UTXOMap = Map[TransactionOutPoint, (TransactionOutput, Seq[Signer.Sign], Option[ScriptPubKey], Option[ScriptWitness], HashType)]

}

object BitcoinTxBuilder {
  private case class BitcoinTxBuilderImpl(
    destinations: Seq[TransactionOutput],
    creditingTxs: Seq[Transaction],
    utxoMap:      UTXOMap,
    feeRate:      FeeUnit,
    changeSPK:    ScriptPubKey,
    network:      BitcoinNetwork
  ) extends BitcoinTxBuilder {
    require(outPoints.exists(o => creditingTxs.exists(_.txId == o.txId)))
  }

  private val logger = BitcoinSLogger.logger

  /**
   * @param destinations where the money is going in the signed tx
   * @param creditingTxs the [[Transaction]]'s that are funding this tx
   * @param utxos extra information needed to spend the outputs in the creditingTxs
   * @param feeRate the desired fee rate for this tx
   * @param changeSPK where we should send the change from the creditingTxs
   * @return either a instance of a [[TxBuilder]],
   *         from which you can call [[TxBuilder.sign]] to generate a signed tx,
   *         or a [[TxBuilderError]]
   */
  def apply(destinations: Seq[TransactionOutput], creditingTxs: Seq[Transaction],
            utxos: UTXOMap, feeRate: FeeUnit, changeSPK: ScriptPubKey, network: BitcoinNetwork): Either[TxBuilder, TxBuilderError] = {
    if (feeRate.toLong <= 0) {
      Right(TxBuilderError.LowFee)
    } else if (utxos.keys.map(o => creditingTxs.exists(_.txId == o.txId)).exists(_ == false)) {
      //means that we did not pass in a crediting tx for an outpoint in utxoMap
      Right(TxBuilderError.MissingCreditingTx)
    } else {
      Left(BitcoinTxBuilderImpl(destinations, creditingTxs, utxos, feeRate, changeSPK, network))
    }
  }

  def apply(destinations: Seq[TransactionOutput], creditingTxs: Seq[Transaction],
            utxos: Seq[UTXOTuple], feeRate: FeeUnit, changeSPK: ScriptPubKey,
            network: BitcoinNetwork): Either[TxBuilder, TxBuilderError] = {
    @tailrec
    def loop(utxos: Seq[UTXOTuple], accum: UTXOMap): UTXOMap = utxos match {
      case Nil => accum
      case h :: t =>
        val result = accum.updated(h._1, (h._2, h._3, h._4, h._5, h._6))
        loop(t, result)
    }
    val map = loop(utxos, Map.empty)
    BitcoinTxBuilder(destinations, creditingTxs, map, feeRate, changeSPK, network)
  }

  /** Runs various sanity checks on the final version of the signed transaction from TxBuilder */
  def sanityChecks(txBuilder: TxBuilder, signedTx: Transaction): Option[TxBuilderError] = {
    val sanityDestination = sanityDestinationChecks(txBuilder, signedTx)
    if (sanityDestination.isDefined) {
      sanityDestination
    } else {
      sanityAmountChecks(txBuilder, signedTx)
    }
  }

  /** Checks that we did not lose a [[TransactionOutput]] in the signing process of this transaction */
  def sanityDestinationChecks(txBuilder: TxBuilder, signedTx: Transaction): Option[TxBuilderError] = {
    //make sure we send coins to the appropriate destinations
    val isMissingDestination = txBuilder.destinations.map(o => signedTx.outputs.contains(o)).exists(_ == false)
    val hasExtraOutputs = if (signedTx.outputs.size == txBuilder.destinations.size) {
      false
    } else {
      //the extra output should be the changeOutput
      !(signedTx.outputs.size == (txBuilder.destinations.size + 1) &&
        signedTx.outputs.map(_.scriptPubKey).contains(txBuilder.changeSPK))
    }
    val spendingTxOutPoints = signedTx.inputs.map(_.previousOutput)
    val hasExtraOutPoints = txBuilder.outPoints.map(o => spendingTxOutPoints.contains(o)).exists(_ == false)
    if (isMissingDestination) {
      Some(TxBuilderError.MissingDestinationOutput)
    } else if (hasExtraOutputs) {
      Some(TxBuilderError.ExtraOutputsAdded)
    } else if (hasExtraOutPoints) {
      Some(TxBuilderError.ExtraOutPoints)
    } else {
      None
    }
  }

  /**
   * Checks that the [[TxBuilder.creditingAmount]] >= [[TxBuilder.destinationAmount]]
   * and then does a sanity check on the tx's fee
   */
  def sanityAmountChecks(txBuilder: TxBuilder, signedTx: Transaction): Option[TxBuilderError] = {
    val spentAmount: CurrencyUnit = signedTx.outputs.map(_.value).fold(CurrencyUnits.zero)(_ + _)
    val creditingAmount = txBuilder.creditingAmount
    val actualFee = creditingAmount - spentAmount
    val estimatedFee = txBuilder.feeRate * signedTx
    if (spentAmount > creditingAmount) {
      Some(TxBuilderError.MintsMoney)
    } else if (actualFee > txBuilder.largestFee) {
      Some(TxBuilderError.HighFee)
    } else if (signedTx.outputs.filterNot(_.scriptPubKey.asm.contains(OP_RETURN))
      .map(_.value).exists(_ < Policy.dustThreshold)) {
      Some(TxBuilderError.OutputBelowDustThreshold)
    } else {
      val feeResult = isValidFeeRange(estimatedFee, actualFee, txBuilder.feeRate)
      feeResult
    }
  }

  /**
   * Checks if the fee is within a 'valid' range
   * @param estimatedFee the estimated amount of fee we should pay
   * @param actualFee the actual amount of fee the transaction pays
   * @param feeRate the fee rate in satoshis/vbyte we paid per byte on this tx
   * @return
   */
  def isValidFeeRange(estimatedFee: CurrencyUnit, actualFee: CurrencyUnit, feeRate: FeeUnit): Option[TxBuilderError] = {
    //what the number '15' represents is the allowed variance -- in bytes -- between the size of the two
    //versions of signed tx. I believe the two signed version can vary in size because the digital
    //signature might have changed in size. It could become larger or smaller depending on the digital
    //signatures produced
    val acceptableVariance = 15 * feeRate.toLong
    val min = Satoshis(Int64(-acceptableVariance))
    val max = Satoshis(Int64(acceptableVariance))
    val difference = estimatedFee - actualFee
    if (difference <= min) {
      Some(TxBuilderError.HighFee)
    } else if (difference >= max) {
      Some(TxBuilderError.LowFee)
    } else {
      None
    }
  }
}
