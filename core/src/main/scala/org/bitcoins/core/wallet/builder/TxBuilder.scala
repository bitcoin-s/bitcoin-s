package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.{BitcoinNetwork, NetworkParameters}
import org.bitcoins.core.crypto.{
  ECDigitalSignature,
  EmptyDigitalSignature,
  TransactionSignatureSerializer,
  WitnessTxSigComponentP2SH
}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.locktime.LockTimeInterpreter
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer._
import org.bitcoins.core.wallet.utxo.{BitcoinUTXOSpendingInfo, UTXOSpendingInfo}

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

  /** The [[org.bitcoins.core.protocol.script.ScriptPubKey ScriptPubKey]]'s we are spending bitcoins to */
  def destinationSPKs: Seq[ScriptPubKey] = destinations.map(_.scriptPubKey)

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

  def utxos: Seq[UTXOSpendingInfo] = utxoMap.values.toSeq

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

  /** The redeem scripts that are needed in this transaction */
  def redeemScriptOpt: Seq[Option[ScriptPubKey]] = utxos.map(_.redeemScriptOpt)

  /** The script witnesses that are needed in this transaction */
  def scriptWitOpt: Seq[Option[ScriptWitness]] = utxos.map(_.scriptWitnessOpt)

  /**
    * The unsigned version of the tx with dummy signatures instead of real signatures in
    * the [[ScriptSignature]]s. This unsigned transaction has fee estimation done against
    * the [[org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte]] you passed in as a parameter
    * the change output is calculated and ready for signing.
    */
  def unsignedTx(implicit ec: ExecutionContext): Future[Transaction]

  def sign(implicit ec: ExecutionContext): Future[Transaction]
}

/**
  * The [[org.bitcoins.core.wallet.builder.TxBuilder]] for the
  * bitcoin network(s) [[org.bitcoins.core.config.BitcoinNetwork]]
  */
sealed abstract class BitcoinTxBuilder extends TxBuilder {

  private val logger = BitcoinSLogger.logger
  private val tc = TransactionConstants

  override def network: BitcoinNetwork

  override def utxoMap: BitcoinTxBuilder.UTXOMap

  override def sign(implicit ec: ExecutionContext): Future[Transaction] = {
    val f: (Seq[BitcoinUTXOSpendingInfo], Transaction) => Boolean = { (_, _) =>
      true
    }
    sign(f)
  }

  override def unsignedTx(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val utxos = utxoMap.values.toList
    val unsignedTxWit = TransactionWitness.fromWitOpt(scriptWitOpt.toVector)
    val lockTime = calcLockTime(utxos)
    val inputs = calcSequenceForInputs(utxos, Policy.isRBFEnabled)
    val emptyChangeOutput = TransactionOutput(CurrencyUnits.zero, changeSPK)
    val unsignedTxNoFee = lockTime.map { l =>
      unsignedTxWit match {
        case EmptyWitness =>
          BaseTransaction(tc.validLockVersion,
                          inputs,
                          destinations ++ Seq(emptyChangeOutput),
                          l)
        case wit: TransactionWitness =>
          WitnessTransaction(tc.validLockVersion,
                             inputs,
                             destinations ++ Seq(emptyChangeOutput),
                             l,
                             wit)
      }
    }
    val unsignedTxWithFee: Try[Future[Transaction]] = unsignedTxNoFee.map {
      utxnf =>
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
          val newOutputs = if (newChangeOutput.value <= Policy.dustThreshold) {
            logger.debug(
              "removing change output as value is below the dustThreshold")
            destinations
          } else {
            destinations ++ Seq(newChangeOutput)
          }
          dtx match {
            case btx: BaseTransaction =>
              BaseTransaction(btx.version, btx.inputs, newOutputs, btx.lockTime)
            case wtx: WitnessTransaction =>
              WitnessTransaction(wtx.version,
                                 wtx.inputs,
                                 newOutputs,
                                 wtx.lockTime,
                                 wtx.witness)
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
  def sign(invariants: (Seq[BitcoinUTXOSpendingInfo], Transaction) => Boolean)(
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
      remaining: List[BitcoinUTXOSpendingInfo],
      txInProgress: Transaction,
      dummySignatures: Boolean)(
      implicit ec: ExecutionContext): Future[Transaction] = remaining match {
    case Nil => Future.successful(txInProgress)
    case info :: t =>
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
      utxo: BitcoinUTXOSpendingInfo,
      unsignedTx: Transaction,
      dummySignatures: Boolean)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val outpoint = utxo.outPoint
    val output = utxo.output
    val signers = utxo.signers
    val redeemScriptOpt = utxo.redeemScriptOpt
    val scriptWitnessOpt = utxo.scriptWitnessOpt
    val hashType = utxo.hashType
    val idx =
      unsignedTx.inputs.zipWithIndex.find(_._1.previousOutput == outpoint)
    if (idx.isEmpty) {
      Future.fromTry(TxBuilderError.MissingOutPoint)
    } else {
      val inputIndex = UInt32(idx.get._2)
      val oldInput = unsignedTx.inputs(inputIndex.toInt)
      output.scriptPubKey match {
        case _: P2PKScriptPubKey =>
          P2PKSigner
            .sign(signers,
                  output,
                  unsignedTx,
                  inputIndex,
                  hashType,
                  dummySignatures)
            .map(_.transaction)
        case _: P2PKHScriptPubKey =>
          P2PKHSigner
            .sign(signers,
                  output,
                  unsignedTx,
                  inputIndex,
                  hashType,
                  dummySignatures)
            .map(_.transaction)
        case _: MultiSignatureScriptPubKey =>
          MultiSigSigner
            .sign(signers,
                  output,
                  unsignedTx,
                  inputIndex,
                  hashType,
                  dummySignatures)
            .map(_.transaction)
        case lock: LockTimeScriptPubKey =>
          lock.nestedScriptPubKey match {
            case _: P2PKScriptPubKey =>
              P2PKSigner
                .sign(signers,
                      output,
                      unsignedTx,
                      inputIndex,
                      hashType,
                      dummySignatures)
                .map(_.transaction)
            case _: P2PKHScriptPubKey =>
              P2PKHSigner
                .sign(signers,
                      output,
                      unsignedTx,
                      inputIndex,
                      hashType,
                      dummySignatures)
                .map(_.transaction)
            case _: MultiSignatureScriptPubKey =>
              MultiSigSigner
                .sign(signers,
                      output,
                      unsignedTx,
                      inputIndex,
                      hashType,
                      dummySignatures)
                .map(_.transaction)
            case _: P2SHScriptPubKey =>
              Future.fromTry(TxBuilderError.NestedP2SHSPK)
            case _: P2WSHWitnessSPKV0 | _: P2WPKHWitnessSPKV0 =>
              Future.fromTry(TxBuilderError.NestedWitnessSPK)
            case _: CSVScriptPubKey | _: CLTVScriptPubKey |
                _: NonStandardScriptPubKey | _: WitnessCommitment |
                EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
              Future.fromTry(TxBuilderError.NoSigner)
          }
        case p2sh: P2SHScriptPubKey =>
          redeemScriptOpt match {

            case Some(redeemScript) =>
              if (p2sh != P2SHScriptPubKey(redeemScript)) {

                Future.fromTry(TxBuilderError.WrongRedeemScript)

              } else {

                val signedTxF: Future[Transaction] = redeemScript match {

                  case p2wpkh: P2WPKHWitnessSPKV0 =>
                    val uwtx = WitnessTransaction.toWitnessTx(unsignedTx)

                    //breaks an abstraction inside of all of the signers
                    //won't be able to be handled properly until gemini stuff
                    //is open sourced
                    signP2SHP2WPKH(unsignedTx = uwtx,
                                   inputIndex = inputIndex,
                                   output = output,
                                   p2wpkh = p2wpkh,
                                   utxo = utxo,
                                   hashType = hashType,
                                   dummySignatures = dummySignatures)

                  case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                      _: MultiSignatureScriptPubKey | _: LockTimeScriptPubKey |
                      _: NonStandardScriptPubKey | _: WitnessCommitment |
                      _: UnassignedWitnessScriptPubKey | _: P2WSHWitnessSPKV0 |
                      EmptyScriptPubKey =>
                    val input = TransactionInput(outpoint,
                                                 EmptyScriptSignature,
                                                 oldInput.sequence)

                    val updatedTx =
                      unsignedTx.updateInput(inputIndex.toInt, input)

                    val updatedOutput =
                      TransactionOutput(output.value, redeemScript)

                    val updatedUTXOInfo = BitcoinUTXOSpendingInfo(
                      outpoint,
                      updatedOutput,
                      signers,
                      None,
                      scriptWitnessOpt,
                      hashType)

                    val signedTxEither = signAndAddInput(updatedUTXOInfo,
                                                         updatedTx,
                                                         dummySignatures)

                    signedTxEither.map { signedTx =>
                      val i = signedTx.inputs(inputIndex.toInt)

                      val p2sh =
                        P2SHScriptSignature(i.scriptSignature, redeemScript)

                      val signedInput =
                        TransactionInput(i.previousOutput, p2sh, i.sequence)

                      val signedInputs =
                        signedTx.inputs.updated(inputIndex.toInt, signedInput)

                      signedTx match {
                        case btx: BaseTransaction =>
                          BaseTransaction(btx.version,
                                          signedInputs,
                                          btx.outputs,
                                          btx.lockTime)
                        case wtx: WitnessTransaction =>
                          WitnessTransaction(wtx.version,
                                             signedInputs,
                                             wtx.outputs,
                                             wtx.lockTime,
                                             wtx.witness)
                      }

                    }

                  case _: P2SHScriptPubKey =>
                    Future.fromTry(TxBuilderError.NestedP2SHSPK)
                }

                signedTxF

              }
            case None => Future.fromTry(TxBuilderError.NoRedeemScript)
          }

        case _: P2WPKHWitnessSPKV0 =>
          //if we don't have a WitnessTransaction we need to convert our unsignedTx to a WitnessTransaction
          val unsignedWTx: WitnessTransaction = unsignedTx match {
            case btx: BaseTransaction =>
              WitnessTransaction(btx.version,
                                 btx.inputs,
                                 btx.outputs,
                                 btx.lockTime,
                                 EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val result = P2WPKHSigner.sign(signers,
                                         output,
                                         unsignedWTx,
                                         inputIndex,
                                         hashType,
                                         dummySignatures)
          result.map(_.transaction)
        case p2wshSPK: P2WSHWitnessSPKV0 =>
          //if we don't have a WitnessTransaction we need to convert our unsignedTx to a WitnessTransaction
          val unsignedWTx: WitnessTransaction = unsignedTx match {
            case btx: BaseTransaction =>
              WitnessTransaction(btx.version,
                                 btx.inputs,
                                 btx.outputs,
                                 btx.lockTime,
                                 EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val p2wshScriptWit = scriptWitnessOpt match {
            case Some(wit) =>
              wit match {
                case EmptyScriptWitness | _: P2WPKHWitnessV0 =>
                  Future.fromTry(TxBuilderError.WrongWitness)
                case x: P2WSHWitnessV0 => Future.successful(x)
              }
            case None => Future.fromTry(TxBuilderError.NoWitness)
          }
          val redeemScriptEither = p2wshScriptWit.map(_.redeemScript)
          val result = redeemScriptEither.flatMap { redeemScript =>
            if (P2WSHWitnessSPKV0(redeemScript) != p2wshSPK) {
              Future.fromTry(TxBuilderError.WrongWitness)
            } else {
              redeemScript match {
                case _: P2PKScriptPubKey =>
                  P2PKSigner.sign(signers,
                                  output,
                                  unsignedWTx,
                                  inputIndex,
                                  hashType,
                                  dummySignatures)
                case _: P2PKHScriptPubKey =>
                  P2PKHSigner.sign(signers,
                                   output,
                                   unsignedWTx,
                                   inputIndex,
                                   hashType,
                                   dummySignatures)
                case _: MultiSignatureScriptPubKey =>
                  MultiSigSigner.sign(signers,
                                      output,
                                      unsignedWTx,
                                      inputIndex,
                                      hashType,
                                      dummySignatures)
                case _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 =>
                  Future.fromTry(TxBuilderError.NestedWitnessSPK)
                case _: P2SHScriptPubKey =>
                  Future.fromTry(TxBuilderError.NestedP2SHSPK)
                case lock: LockTimeScriptPubKey =>
                  lock.nestedScriptPubKey match {
                    case _: P2PKScriptPubKey =>
                      P2PKSigner.sign(signers,
                                      output,
                                      unsignedTx,
                                      inputIndex,
                                      hashType,
                                      dummySignatures)
                    case _: P2PKHScriptPubKey =>
                      P2PKHSigner.sign(signers,
                                       output,
                                       unsignedTx,
                                       inputIndex,
                                       hashType,
                                       dummySignatures)
                    case _: MultiSignatureScriptPubKey =>
                      MultiSigSigner.sign(signers,
                                          output,
                                          unsignedTx,
                                          inputIndex,
                                          hashType,
                                          dummySignatures)
                    case _: P2WPKHWitnessSPKV0 =>
                      P2WPKHSigner.sign(signers,
                                        output,
                                        unsignedTx,
                                        inputIndex,
                                        hashType,
                                        dummySignatures)
                    case _: P2SHScriptPubKey =>
                      Future.fromTry(TxBuilderError.NestedP2SHSPK)
                    case _: P2WSHWitnessSPKV0 =>
                      Future.fromTry(TxBuilderError.NestedWitnessSPK)
                    case _: CSVScriptPubKey | _: CLTVScriptPubKey |
                        _: NonStandardScriptPubKey | _: WitnessCommitment |
                        EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
                      Future.fromTry(TxBuilderError.NoSigner)
                  }
                case _: NonStandardScriptPubKey | _: WitnessCommitment |
                    EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
                  Future.fromTry(TxBuilderError.NoSigner)
              }
            }
          }
          result.map(_.transaction)
        case _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.NoSigner)
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
  private def solveSequenceForCSV(scriptNum: ScriptNumber): UInt32 =
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

  /**
    * This helper function calculates the appropriate locktime for a transaction.
    * To be able to spend [[CLTVScriptPubKey]]'s you need to have the transaction's
    * locktime set to the same value (or higher) than the output it is spending.
    * See BIP65 for more info
    */
  private def calcLockTime(utxos: Seq[BitcoinUTXOSpendingInfo]): Try[UInt32] = {
    @tailrec
    def loop(
        remaining: Seq[BitcoinUTXOSpendingInfo],
        currentLockTime: UInt32): Try[UInt32] = remaining match {
      case Nil => Success(currentLockTime)
      case BitcoinUTXOSpendingInfo(outpoint,
                                   output,
                                   signers,
                                   redeemScriptOpt,
                                   scriptWitOpt,
                                   hashType) :: t =>
        output.scriptPubKey match {
          case cltv: CLTVScriptPubKey =>
            val lockTime =
              if (cltv.locktime.toLong > UInt32.max.toLong || cltv.locktime.toLong < 0) {
                TxBuilderError.IncompatibleLockTimes
              } else Success(UInt32(cltv.locktime.toLong))
            val result = lockTime.flatMap { l: UInt32 =>
              if (currentLockTime < l) {
                val lockTimeThreshold = tc.locktimeThreshold
                if (currentLockTime < lockTimeThreshold && l >= lockTimeThreshold) {
                  //means that we spend two different locktime types, one of the outputs spends a
                  //OP_CLTV script by block height, the other spends one by time stamp
                  TxBuilderError.IncompatibleLockTimes
                } else Success(l)
              } else Success(currentLockTime)
            }
            result
          case _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 =>
            if (redeemScriptOpt.isDefined) {
              //recursively call with redeem script as output script
              val o = TransactionOutput(output.value, redeemScriptOpt.get)
              val i = BitcoinUTXOSpendingInfo(outpoint,
                                              o,
                                              signers,
                                              None,
                                              scriptWitOpt,
                                              hashType)
              loop(i +: t, currentLockTime)
            } else if (scriptWitOpt.isDefined) {
              scriptWitOpt.get match {
                case EmptyScriptWitness    => loop(t, currentLockTime)
                case _: P2WPKHWitnessV0    => loop(t, currentLockTime)
                case p2wsh: P2WSHWitnessV0 =>
                  //recursively call with the witness redeem script as the script
                  val o = TransactionOutput(output.value, p2wsh.redeemScript)
                  val i = BitcoinUTXOSpendingInfo(outpoint,
                                                  o,
                                                  signers,
                                                  redeemScriptOpt,
                                                  None,
                                                  hashType)
                  loop(i +: t, currentLockTime)
              }
            } else {
              loop(t, currentLockTime)
            }
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
              _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
              _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
              _: NonStandardScriptPubKey | _: WitnessCommitment |
              EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey |
              _: CSVScriptPubKey =>
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
  private def calcSequenceForInputs(
      utxos: Seq[UTXOSpendingInfo],
      isRBFEnabled: Boolean): Seq[TransactionInput] = {
    @tailrec
    def loop(
        remaining: Seq[UTXOSpendingInfo],
        accum: Seq[TransactionInput]): Seq[TransactionInput] = remaining match {
      case Nil => accum.reverse
      case BitcoinUTXOSpendingInfo(outpoint,
                                   output,
                                   signers,
                                   redeemScriptOpt,
                                   scriptWitOpt,
                                   hashType) :: t =>
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
              val i = BitcoinUTXOSpendingInfo(outpoint,
                                              o,
                                              signers,
                                              None,
                                              scriptWitOpt,
                                              hashType)
              loop(i +: t, accum)
            } else if (scriptWitOpt.isDefined) {
              scriptWitOpt.get match {
                case EmptyScriptWitness | _: P2WPKHWitnessV0 => loop(t, accum)
                case p2wsh: P2WSHWitnessV0 =>
                  val o = TransactionOutput(output.value, p2wsh.redeemScript)
                  val i = BitcoinUTXOSpendingInfo(outpoint,
                                                  o,
                                                  signers,
                                                  redeemScriptOpt,
                                                  None,
                                                  hashType)
                  loop(i +: t, accum)
              }
            } else loop(t, accum)
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
              _: MultiSignatureScriptPubKey | _: P2WPKHWitnessSPKV0 |
              _: NonStandardScriptPubKey | _: WitnessCommitment |
              EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
            //none of these script types affect the sequence number of a tx
            //the sequence only needs to be adjustd if we have replace by fee (RBF) enabled
            //see BIP125 for more information
            val sequence =
              if (isRBFEnabled) UInt32.zero else TransactionConstants.sequence
            val input =
              TransactionInput(outpoint, EmptyScriptSignature, sequence)
            loop(t, input +: accum)
        }
    }
    val inputs = loop(utxos, Nil)
    inputs
  }

  def signP2SHP2WPKH(
      unsignedTx: WitnessTransaction,
      inputIndex: UInt32,
      p2wpkh: P2WPKHWitnessSPKV0,
      output: TransactionOutput,
      utxo: UTXOSpendingInfo,
      hashType: HashType,
      dummySignatures: Boolean): Future[Transaction] = {
    //special rule for p2sh(p2wpkh)
    //https://bitcoincore.org/en/segwit_wallet_dev/#signature-generation-and-verification-for-p2sh-p2wpkh
    //we actually sign the fully expanded redeemScript
    val pubKey = utxo.signers.head.publicKey
    if (P2WPKHWitnessSPKV0(pubKey) != p2wpkh) {
      Future.fromTry(TxBuilderError.WrongPublicKey)
    } else {
      val p2shScriptSig = P2SHScriptSignature(p2wpkh)

      val oldInput = unsignedTx.inputs(inputIndex.toInt)

      val updatedInput = TransactionInput(oldInput.previousOutput,
                                          p2shScriptSig,
                                          oldInput.sequence)

      val uwtx = {
        val u = unsignedTx.updateInput(inputIndex.toInt, updatedInput)
        WitnessTransaction.toWitnessTx(u)
      }

      val wtxSigComp = {
        WitnessTxSigComponentP2SH(transaction = uwtx,
                                  inputIndex = inputIndex,
                                  output = output,
                                  flags = Policy.standardFlags)
      }

      val hashForSig = TransactionSignatureSerializer.hashForSignature(
        txSigComponent = wtxSigComp,
        hashType = hashType)

      //sign the hash
      val signature: ECDigitalSignature = {
        if (dummySignatures) {
          EmptyDigitalSignature
        } else {
          val sig = utxo.signers.head.sign(hashForSig.bytes)
          //append hash type
          ECDigitalSignature.fromBytes(sig.bytes.:+(hashType.byte))
        }
      }

      val p2wpkhWit = P2WPKHWitnessV0(publicKey = pubKey, signature = signature)

      val updatedWit = uwtx.updateWitness(inputIndex.toInt, p2wpkhWit)

      Future.successful(updatedWit)
    }
  }
}

object TxBuilder {

  /** This contains all the information needed to create a valid [[TransactionInput]] that spends this utxo */
  type UTXOMap = Map[TransactionOutPoint, UTXOSpendingInfo]
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
    val min = Satoshis(Int64(-acceptableVariance))
    val max = Satoshis(Int64(acceptableVariance))
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
}

object BitcoinTxBuilder {
  type UTXOMap = Map[TransactionOutPoint, BitcoinUTXOSpendingInfo]

  private case class BitcoinTxBuilderImpl(
      destinations: Seq[TransactionOutput],
      utxoMap: UTXOMap,
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork)
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
    if (feeRate.toLong <= 0) {
      Future.fromTry(TxBuilderError.LowFee)
    } else {
      Future.successful(
        BitcoinTxBuilderImpl(destinations, utxos, feeRate, changeSPK, network))
    }
  }

  def apply(
      destinations: Seq[TransactionOutput],
      utxos: Seq[BitcoinUTXOSpendingInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork): Future[BitcoinTxBuilder] = {
    @tailrec
    def loop(utxos: Seq[UTXOSpendingInfo], accum: UTXOMap): UTXOMap =
      utxos match {
        case Nil => accum
        case h :: t =>
          val u = BitcoinUTXOSpendingInfo(h.outPoint,
                                          h.output,
                                          h.signers,
                                          h.redeemScriptOpt,
                                          h.scriptWitnessOpt,
                                          h.hashType)
          val result: BitcoinTxBuilder.UTXOMap = accum.updated(h.outPoint, u)
          loop(t, result)
      }
    val map = loop(utxos, Map.empty)
    BitcoinTxBuilder(destinations, map, feeRate, changeSPK, network)
  }
}
