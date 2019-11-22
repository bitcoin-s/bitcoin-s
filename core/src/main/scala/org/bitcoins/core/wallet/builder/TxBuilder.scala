package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.{BitcoinNetwork, NetworkParameters}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer._
import org.bitcoins.core.wallet.utxo._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
  * High level class to create a signed transaction that spends a set of
  * unspent transaction outputs.
  *
  * The most important method in this class is the 'sign' method. This will start the signing procedure for a
  * transaction and then return either a signed [[org.bitcoins.core.protocol.transaction.Transaction Transaction]]
  * or a [[org.bitcoins.core.wallet.builder.TxBuilderError TxBuilderError]]
  *
  * For usage examples see TxBuilderSpec
  */
sealed abstract class TxBuilder {

  /** The outputs which we are spending bitcoins to */
  def destinations: Seq[TransactionOutput]

  /** A sequence of the amounts we are spending in this transaction */
  def destinationAmounts: Seq[CurrencyUnit] = destinations.map(_.value)

  /** The spent amount of bitcoins we are sending in the transaction, this does NOT include the fee */
  def destinationAmount: CurrencyUnit =
    destinationAmounts.fold(CurrencyUnits.zero)(_ + _)

  /** The total amount of satoshis that are able to be spent by this transaction */
  def creditingAmount: CurrencyUnit =
    utxos.map(_.output.value).fold(CurrencyUnits.zero)(_ + _)

  /** The largest possible fee in this transaction could pay */
  def largestFee: CurrencyUnit = creditingAmount - destinationAmount

  /**
    * The list of [[org.bitcoins.core.protocol.transaction.TransactionOutPoint TransactionOutPoint]]s we are
    * attempting to spend
    * and the signers, redeem scripts, and script witnesses that might be needed to spend this outpoint.
    * This information is dependent on what the [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]
    * type is we are spending. For isntance, if we are spending a
    * regular [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey P2PKHScriptPubKey]], we do not need a
    * redeem script or script witness.
    *
    * If we are spending a [[org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0 P2WPKHWitnessSPKV0]] we do not
    * need a redeem script, but we need a [[org.bitcoins.core.protocol.script.ScriptWitness ScriptWitness]]
    */
  def utxoMap: TxBuilder.UTXOMap

  def utxos: Seq[UTXOSpendingInfoFull] = utxoMap.values.toSeq

  /** This represents the rate, in [[org.bitcoins.core.wallet.fee.FeeUnit FeeUnit]], we
    * should pay for this transaction */
  def feeRate: FeeUnit

  /**
    * This is where all the money that is NOT sent to destination outputs is spent too.
    * If we don't specify a change output, a large miner fee may be paid as more than likely
    * the difference between  `creditingAmount` and `destinationAmount` is not a market rate miner fee
    */
  def changeSPK: ScriptPubKey

  /**
    * The network that this [[org.bitcoins.core.wallet.builder.TxBuilder TxBuilder]] is signing a transaction for.
    * An example could be [[org.bitcoins.core.config.MainNet MainNet]]
    */
  def network: NetworkParameters

  /** The outpoints that we are using in this transaction */
  def outPoints: Seq[TransactionOutPoint] = utxoMap.keys.toSeq

  /** The script witnesses that are needed in this transaction */
  def scriptWitOpt: Seq[Option[ScriptWitness]] = utxos.map(_.scriptWitnessOpt)

  def lockTimeOverrideOpt: Option[UInt32]

  /**
    * The unsigned version of the tx with dummy signatures instead of real signatures in
    * the [[org.bitcoins.core.protocol.script.ScriptSignature ScriptSignature]]s.
    * This unsigned transaction has fee estimation done against
    * the [[org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte SatoshisPerVirtualByte]]
    * you passed in as a parameter
    * the change output is calculated and ready for signing.
    */
  def unsignedTx(implicit ec: ExecutionContext): Future[Transaction]

  def sign(implicit ec: ExecutionContext): Future[Transaction]
}

/**
  * The [[org.bitcoins.core.wallet.builder.TxBuilder TxBuilder]] for the
  * bitcoin network(s) [[org.bitcoins.core.config.BitcoinNetwork BitcoinNetwork]]
  */
sealed abstract class BitcoinTxBuilder extends TxBuilder {

  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants

  override def network: BitcoinNetwork

  override def utxoMap: BitcoinTxBuilder.UTXOMap

  override def sign(implicit ec: ExecutionContext): Future[Transaction] = {
    val f: (Seq[BitcoinUTXOSpendingInfoFull], Transaction) => Boolean = {
      (_, _) =>
        true
    }
    sign(f)
  }

  override def unsignedTx(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val utxos = utxoMap.values.toList
    val unsignedTxWit = TransactionWitness.fromWitOpt(scriptWitOpt.toVector)
    val calculatedLockTime = TxBuilder.calcLockTime(utxos)
    val inputs = TxBuilder.calcSequenceForInputs(utxos)
    val emptyChangeOutput = TransactionOutput(CurrencyUnits.zero, changeSPK)
    val unsignedTxNoFee = calculatedLockTime.map { cLockTime =>
      val lockTimeF = lockTimeOverrideOpt match {
        case None => Future.successful(cLockTime)
        case Some(overrideLockTime) =>
          if (overrideLockTime >= cLockTime) {
            Future.successful(overrideLockTime)
          } else {
            Future.failed(new IllegalStateException(
              s"Specified locktime ($overrideLockTime) is smaller than the locktime computed from inputs ($cLockTime)"
            ))
          }
      }

      lockTimeF.map { lockTime =>
        unsignedTxWit match {
          case _: EmptyWitness =>
            BaseTransaction(tc.validLockVersion,
                            inputs,
                            destinations ++ Seq(emptyChangeOutput),
                            lockTime)
          case wit: TransactionWitness =>
            WitnessTransaction(tc.validLockVersion,
                               inputs,
                               destinations ++ Seq(emptyChangeOutput),
                               lockTime,
                               wit)
        }
      }
    }
    val unsignedTxWithFee: Try[Future[Transaction]] = unsignedTxNoFee.map {
      utxnfF =>
        utxnfF.flatMap { utxnf =>
          val dummySignTx = loop(utxos, utxnf, true)
          dummySignTx.map { dtx =>
            logger.debug(s"dummySignTx $dtx")
            val fee = feeRate.calc(dtx)
            logger.debug(s"fee $fee")
            val change = creditingAmount - destinationAmount - fee
            val newChangeOutput = TransactionOutput(change, changeSPK)
            logger.debug(s"newChangeOutput $newChangeOutput")
            //if the change output is below the dust threshold after calculating the fee, don't add it
            //to the tx
            val newOutputs =
              if (newChangeOutput.value <= Policy.dustThreshold) {
                logger.debug(
                  "removing change output as value is below the dustThreshold")
                destinations
              } else {
                destinations ++ Seq(newChangeOutput)
              }
            dtx match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,
                                btx.inputs,
                                newOutputs,
                                btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,
                                   wtx.inputs,
                                   newOutputs,
                                   wtx.lockTime,
                                   wtx.witness)
            }
          }
        }
    }
    Future.fromTry(unsignedTxWithFee).flatMap(g => g)
  }

  /**
    * Signs the given transaction and then returns a signed tx that spends
    * all of the given outputs.
    * Checks the given invariants when the signing process is done
    * An example of some invariants is that the fee on the signed transaction below a certain amount,
    * or that RBF is enabled on the signed transaction.
    *
    * @param invariants - invariants that should hold true when we are done signing the transaction
    * @return the signed transaction, or a [[TxBuilderError]] indicating what went wrong when signing the tx
    */
  def sign(
      invariants: (Seq[BitcoinUTXOSpendingInfoFull], Transaction) => Boolean)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val utxos = utxoMap.values.toList
    val signedTxWithFee = unsignedTx.flatMap { utx: Transaction =>
      //sign the tx for this time
      val signedTx = loop(utxos, utx, false)
      signedTx.flatMap { tx =>
        val t: Try[Transaction] = {
          if (invariants(utxos, tx)) {
            //final sanity checks
            TxBuilder.sanityChecks(this, tx) match {
              case Success(_)   => Success(tx)
              case Failure(err) => Failure(err)
            }
          } else {
            TxBuilderError.FailedUserInvariants
          }
        }
        Future.fromTry(t)
      }
    }
    signedTxWithFee
  }

  private def loop(
      remaining: List[BitcoinUTXOSpendingInfoFull],
      txInProgress: Transaction,
      dummySignatures: Boolean)(
      implicit ec: ExecutionContext): Future[Transaction] = remaining match {
    case Nil => Future.successful(txInProgress)
    case info +: t =>
      val partiallySigned = signAndAddInput(info, txInProgress, dummySignatures)
      partiallySigned.flatMap(tx => loop(t, tx, dummySignatures))
  }

  /**
    * This function creates a newly signed input, and then adds it to the unsigned transaction
    * @param utxo - the information needed to validly spend the given output
    * @param unsignedTx - the transaction that we are spending this output in
    * @return either the transaction with the signed input added, or a [[TxBuilderError]]
    */
  private def signAndAddInput(
      utxo: BitcoinUTXOSpendingInfoFull,
      unsignedTx: Transaction,
      dummySignatures: Boolean)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val idx =
      unsignedTx.inputs.zipWithIndex.find(_._1.previousOutput == utxo.outPoint)
    if (idx.isEmpty) {
      Future.fromTry(TxBuilderError.MissingOutPoint)
    } else {
      utxo match {
        case _: UnassignedSegwitNativeUTXOSpendingInfo =>
          Future.fromTry(TxBuilderError.NoSigner)
        case _: BitcoinUTXOSpendingInfoFull =>
          BitcoinSigner
            .sign(utxo, unsignedTx, dummySignatures)
            .map(_.transaction)
      }
    }
  }
}

object TxBuilder {

  /** This contains all the information needed to create a valid
    * [[org.bitcoins.core.protocol.transaction.TransactionInput TransactionInput]] that spends this utxo */
  type UTXOMap = Map[TransactionOutPoint, UTXOSpendingInfoFull]
  private val logger = BitcoinSLogger.logger

  /** Runs various sanity checks on the final version of the signed transaction from TxBuilder */
  def sanityChecks(txBuilder: TxBuilder, signedTx: Transaction): Try[Unit] = {
    val sanityDestination = sanityDestinationChecks(txBuilder, signedTx)
    if (sanityDestination.isFailure) {
      sanityDestination
    } else {
      sanityAmountChecks(txBuilder, signedTx)
    }
  }

  /** Checks that we did not lose a [[org.bitcoins.core.protocol.transaction.TransactionOutput TransactionOutput]]
    * in the signing process of this transaction */
  def sanityDestinationChecks(
      txBuilder: TxBuilder,
      signedTx: Transaction): Try[Unit] = {
    //make sure we send coins to the appropriate destinations
    val isMissingDestination = txBuilder.destinations
      .map(o => signedTx.outputs.contains(o))
      .exists(_ == false)
    val hasExtraOutputs =
      if (signedTx.outputs.size == txBuilder.destinations.size) {
        false
      } else {
        //the extra output should be the changeOutput
        !(signedTx.outputs.size == (txBuilder.destinations.size + 1) &&
          signedTx.outputs.map(_.scriptPubKey).contains(txBuilder.changeSPK))
      }
    val spendingTxOutPoints = signedTx.inputs.map(_.previousOutput)
    val hasExtraOutPoints = txBuilder.outPoints
      .map(o => spendingTxOutPoints.contains(o))
      .exists(_ == false)
    if (isMissingDestination) {
      TxBuilderError.MissingDestinationOutput
    } else if (hasExtraOutputs) {
      TxBuilderError.ExtraOutputsAdded
    } else if (hasExtraOutPoints) {
      TxBuilderError.ExtraOutPoints
    } else {
      Success(())
    }
  }

  /**
    * Checks that the [[org.bitcoins.core.wallet.builder.TxBuilder.creditingAmount TxBuilder.creditingAmount]]
    * >= [[org.bitcoins.core.wallet.builder.TxBuilder.destinationAmount TxBuilder.destinationAmount]]
    * and then does a sanity check on the tx's fee
    */
  def sanityAmountChecks(
      txBuilder: TxBuilder,
      signedTx: Transaction): Try[Unit] = {
    val spentAmount: CurrencyUnit =
      signedTx.outputs.map(_.value).fold(CurrencyUnits.zero)(_ + _)
    val creditingAmount = txBuilder.creditingAmount
    val actualFee = creditingAmount - spentAmount
    val estimatedFee = txBuilder.feeRate * signedTx
    if (spentAmount > creditingAmount) {
      TxBuilderError.MintsMoney
    } else if (actualFee > txBuilder.largestFee) {
      TxBuilderError.HighFee
    } else if (signedTx.outputs
                 .filterNot(_.scriptPubKey.asm.contains(OP_RETURN))
                 .map(_.value)
                 .exists(_ < Policy.dustThreshold)) {
      TxBuilderError.OutputBelowDustThreshold
    } else {
      val feeResult =
        isValidFeeRange(estimatedFee, actualFee, txBuilder.feeRate)
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
  def isValidFeeRange(
      estimatedFee: CurrencyUnit,
      actualFee: CurrencyUnit,
      feeRate: FeeUnit): Try[Unit] = {

    //what the number '40' represents is the allowed variance -- in bytes -- between the size of the two
    //versions of signed tx. I believe the two signed version can vary in size because the digital
    //signature might have changed in size. It could become larger or smaller depending on the digital
    //signatures produced.

    //Personally I think 40 seems like a little high. As you shouldn't vary more than a 2 bytes per input in the tx i think?
    //bumping for now though as I don't want to spend time debugging
    //I think there is something incorrect that errors to the low side of fee estimation
    //for p2sh(p2wpkh) txs

    //See this link for more info on variance in size on ECDigitalSignatures
    //https://en.bitcoin.it/wiki/Elliptic_Curve_Digital_Signature_Algorithm

    val acceptableVariance = 40 * feeRate.toLong
    val min = Satoshis(-acceptableVariance)
    val max = Satoshis(acceptableVariance)
    val difference = estimatedFee - actualFee
    if (difference <= min) {
      logger.error(
        s"Fee was too high. Estimated fee ${estimatedFee}, actualFee ${actualFee}, difference ${difference}, acceptableVariance ${acceptableVariance}")
      TxBuilderError.HighFee
    } else if (difference >= max) {
      logger.error(
        s"Fee was too low. Estimated fee ${estimatedFee}, actualFee ${actualFee}, difference ${difference}, acceptableVariance ${acceptableVariance}")

      TxBuilderError.LowFee
    } else {
      Success(())
    }
  }

  /**
    * This helper function calculates the appropriate locktime for a transaction.
    * To be able to spend [[CLTVScriptPubKey]]'s you need to have the transaction's
    * locktime set to the same value (or higher) than the output it is spending.
    * See BIP65 for more info
    */
  def calcLockTime(utxos: Seq[BitcoinUTXOSpendingInfoFull]): Try[UInt32] = {
    def computeNextLockTime(
        currentLockTimeOpt: Option[UInt32],
        locktime: Long): Try[UInt32] = {
      val lockTimeT =
        if (locktime > UInt32.max.toLong || locktime < 0) {
          TxBuilderError.IncompatibleLockTimes
        } else Success(UInt32(locktime))
      lockTimeT.flatMap { lockTime: UInt32 =>
        currentLockTimeOpt match {
          case Some(currentLockTime) =>
            val lockTimeThreshold = TransactionConstants.locktimeThreshold
            if (currentLockTime < lockTime) {
              if (currentLockTime < lockTimeThreshold && lockTime >= lockTimeThreshold) {
                //means that we spend two different locktime types, one of the outputs spends a
                //OP_CLTV script by block height, the other spends one by time stamp
                TxBuilderError.IncompatibleLockTimes
              } else Success(lockTime)
            } else if (currentLockTime >= lockTimeThreshold && lockTime < lockTimeThreshold) {
              //means that we spend two different locktime types, one of the outputs spends a
              //OP_CLTV script by block height, the other spends one by time stamp
              TxBuilderError.IncompatibleLockTimes
            } else {
              Success(currentLockTime)
            }
          case None => Success(lockTime)
        }
      }
    }

    @tailrec
    def loop(
        remaining: Seq[BitcoinUTXOSpendingInfoFull],
        currentLockTimeOpt: Option[UInt32]): Try[UInt32] =
      remaining match {
        case Nil =>
          Success(currentLockTimeOpt.getOrElse(TransactionConstants.lockTime))
        case spendingInfo +: newRemaining =>
          spendingInfo match {
            case lockTime: LockTimeSpendingInfoFull =>
              lockTime.scriptPubKey match {
                case _: CSVScriptPubKey =>
                  loop(newRemaining, currentLockTimeOpt)
                case cltv: CLTVScriptPubKey =>
                  val result = computeNextLockTime(currentLockTimeOpt,
                                                   cltv.locktime.toLong)

                  result match {
                    case Success(newLockTime) =>
                      loop(newRemaining, Some(newLockTime))
                    case _: Failure[UInt32] => result
                  }
              }
            case p2sh: P2SHSpendingInfoFull =>
              loop(p2sh.nestedSpendingInfo +: newRemaining, currentLockTimeOpt)
            case p2wsh: P2WSHV0SpendingInfoFull =>
              loop(p2wsh.nestedSpendingInfo +: newRemaining, currentLockTimeOpt)
            case conditional: ConditionalSpendingInfoFull =>
              loop(conditional.nestedSpendingInfo +: newRemaining,
                   currentLockTimeOpt)
            case _: P2WPKHV0SpendingInfo | _: P2PKWithTimeoutSpendingInfo |
                _: UnassignedSegwitNativeUTXOSpendingInfo |
                _: P2PKSpendingInfo | _: P2PKHSpendingInfo |
                _: MultiSignatureSpendingInfoFull | _: EmptySpendingInfo =>
              // none of these scripts affect the locktime of a tx
              loop(newRemaining, currentLockTimeOpt)
          }
      }

    loop(utxos, None)
  }

  /**
    * This helper function calculates the appropriate sequence number for each transaction input.
    * [[CLTVScriptPubKey]] and [[CSVScriptPubKey]]'s need certain sequence numbers on the inputs
    * to make them spendable.
    * See BIP68/112 and BIP65 for more info
    */
  def calcSequenceForInputs(
      utxos: Seq[UTXOSpendingInfo],
      defaultSequence: UInt32 = Policy.sequence): Seq[TransactionInput] = {
    @tailrec
    def loop(
        remaining: Seq[UTXOSpendingInfo],
        accum: Seq[TransactionInput]): Seq[TransactionInput] =
      remaining match {
        case Nil => accum.reverse
        case spendingInfo +: newRemaining =>
          spendingInfo match {
            case lockTime: LockTimeSpendingInfo =>
              val sequence = lockTime.scriptPubKey match {
                case csv: CSVScriptPubKey => solveSequenceForCSV(csv.locktime)
                case _: CLTVScriptPubKey  => UInt32.zero
              }
              val input = TransactionInput(lockTime.outPoint,
                                           EmptyScriptSignature,
                                           sequence)
              loop(newRemaining, input +: accum)
            case p2pkWithTimeout: P2PKWithTimeoutSpendingInfo =>
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
            case p2sh: P2SHSpendingInfo =>
              loop(p2sh.nestedSpendingInfo +: newRemaining, accum)
            case p2wsh: P2WSHV0SpendingInfo =>
              loop(p2wsh.nestedSpendingInfo +: newRemaining, accum)
            case conditional: ConditionalSpendingInfo =>
              loop(conditional.nestedSpendingInfo +: newRemaining, accum)
            case _: P2WPKHV0SpendingInfo |
                _: UnassignedSegwitNativeUTXOSpendingInfo |
                _: P2PKSpendingInfo | _: P2PKHSpendingInfo |
                _: MultiSignatureSpendingInfo | _: EmptySpendingInfo =>
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
    * Returns a valid sequence number for the given [[ScriptNumber]]
    * A transaction needs a valid sequence number to spend a OP_CHECKSEQUENCEVERIFY script.
    * See BIP68/112 for more information
    * [[https://github.com/bitcoin/bips/blob/master/bip-0068.mediawiki]]
    * [[https://github.com/bitcoin/bips/blob/master/bip-0112.mediawiki]]
    */
  def solveSequenceForCSV(scriptNum: ScriptNumber): UInt32 =
    LockTimeInterpreter.isCSVLockByBlockHeight(scriptNum) match {
      case true =>
        //means that we need to have had scriptNum blocks bassed since this tx was included a block to be able to spend this output
        val blocksPassed = scriptNum.toLong & TransactionConstants.sequenceLockTimeMask.toLong
        val sequence = UInt32(blocksPassed)
        sequence
      case false =>
        //means that we need to have had 512 * n seconds passed since the tx was included in a block passed
        val n = scriptNum.toLong
        val sequence = UInt32(
          n & TransactionConstants.sequenceLockTimeMask.toLong)
        //set sequence number to indicate this is relative locktime
        sequence | TransactionConstants.sequenceLockTimeTypeFlag
    }
}

object BitcoinTxBuilder {
  type UTXOMap = Map[TransactionOutPoint, BitcoinUTXOSpendingInfoFull]

  private case class BitcoinTxBuilderImpl(
      destinations: Seq[TransactionOutput],
      utxoMap: UTXOMap,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork,
      lockTimeOverrideOpt: Option[UInt32])
      extends BitcoinTxBuilder

  /**
    * @param destinations where the money is going in the signed tx
    * @param utxos extra information needed to spend the outputs in the creditingTxs
    * @param feeRate the desired fee rate for this tx
    * @param changeSPK where we should send the change from the creditingTxs
    * @return either a instance of a [[org.bitcoins.core.wallet.builder.TxBuilder TxBuilder]],
    *         from which you can call [[org.bitcoins.core.wallet.builder.TxBuilder.sign TxBuilder.sign]]
    *         to generate a signed tx, or a
    *         [[org.bitcoins.core.wallet.builder.TxBuilderError TxBuilderError]]
    */
  def apply(
      destinations: Seq[TransactionOutput],
      utxos: BitcoinTxBuilder.UTXOMap,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork): Future[BitcoinTxBuilder] = {
    BitcoinTxBuilder(destinations,
                     utxos,
                     feeRate,
                     changeSPK,
                     network,
                     lockTimeOverrideOpt = None)
  }

  def apply(
      destinations: Seq[TransactionOutput],
      utxos: BitcoinTxBuilder.UTXOMap,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork,
      lockTimeOverride: UInt32): Future[BitcoinTxBuilder] = {
    BitcoinTxBuilder(destinations,
                     utxos,
                     feeRate,
                     changeSPK,
                     network,
                     Some(lockTimeOverride))
  }

  def apply(
      destinations: Seq[TransactionOutput],
      utxos: BitcoinTxBuilder.UTXOMap,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork,
      lockTimeOverrideOpt: Option[UInt32]): Future[BitcoinTxBuilder] = {
    if (feeRate.toLong <= 0) {
      Future.fromTry(TxBuilderError.LowFee)
    } else {
      Future.successful(
        BitcoinTxBuilderImpl(destinations,
                             utxos,
                             feeRate,
                             changeSPK,
                             network,
                             lockTimeOverrideOpt))
    }
  }

  def apply(
      destinations: Seq[TransactionOutput],
      utxos: Seq[BitcoinUTXOSpendingInfoFull],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork): Future[BitcoinTxBuilder] = {
    BitcoinTxBuilder(destinations,
                     utxos,
                     feeRate,
                     changeSPK,
                     network,
                     lockTimeOverrideOpt = None)
  }

  def apply(
      destinations: Seq[TransactionOutput],
      utxos: Seq[BitcoinUTXOSpendingInfoFull],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork,
      lockTimeOverride: UInt32): Future[BitcoinTxBuilder] = {
    BitcoinTxBuilder(destinations,
                     utxos,
                     feeRate,
                     changeSPK,
                     network,
                     Some(lockTimeOverride))
  }

  def apply(
      destinations: Seq[TransactionOutput],
      utxos: Seq[BitcoinUTXOSpendingInfoFull],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork,
      lockTimeOverrideOpt: Option[UInt32]): Future[BitcoinTxBuilder] = {
    require(utxos.groupBy(_.outPoint).values.forall(_.length == 1),
            "Cannot have multiple UTXOSpendingInfos spending the same UTXO")
    @tailrec
    def loop(utxos: Seq[UTXOSpendingInfoFull], accum: UTXOMap): UTXOMap =
      utxos match {
        case Nil => accum
        case h +: t =>
          val u = BitcoinUTXOSpendingInfoFull(
            outPoint = h.outPoint,
            output = h.output,
            signers = h.signers,
            redeemScriptOpt = h.redeemScriptOpt,
            scriptWitnessOpt = h.scriptWitnessOpt,
            hashType = h.hashType,
            conditionalPath = h.conditionalPath
          )
          val result: BitcoinTxBuilder.UTXOMap = accum.updated(h.outPoint, u)
          loop(t, result)
      }
    val map = loop(utxos, Map.empty)
    BitcoinTxBuilder(destinations,
                     map,
                     feeRate,
                     changeSPK,
                     network,
                     lockTimeOverrideOpt)
  }

  /**
    * Sets the ScriptSignature for every input in the given transaction to an EmptyScriptSignature
    * as well as sets the witness to an EmptyWitness
    * @param tx Transaction to empty signatures
    * @return Transaction with no signatures
    */
  def emptyAllScriptSigs(tx: Transaction): Transaction = {
    val newInputs = tx.inputs.map { input =>
      TransactionInput(input.previousOutput,
                       EmptyScriptSignature,
                       input.sequence)
    }

    tx match {
      case btx: BaseTransaction =>
        BaseTransaction(version = btx.version,
                        inputs = newInputs,
                        outputs = btx.outputs,
                        lockTime = btx.lockTime)
      case wtx: WitnessTransaction =>
        WitnessTransaction(version = wtx.version,
                           inputs = newInputs,
                           outputs = wtx.outputs,
                           lockTime = wtx.lockTime,
                           witness = EmptyWitness.fromInputs(newInputs))
    }
  }
}
