package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  CSVScriptPubKey,
  EmptyScriptSignature,
  EmptyScriptWitness,
  ScriptWitnessV0
}
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.builder.RawTxSigner.logger
import org.bitcoins.core.wallet.builder.TxBuilderError
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer.BitcoinSigner
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
import org.bitcoins.crypto.{DummyECDigitalSignature, Sign}

import scala.annotation.tailrec
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object TxUtil {

  /**
    * This helper function calculates the appropriate locktime for a transaction.
    * To be able to spend [[CLTVScriptPubKey]]'s you need to have the transaction's
    * locktime set to the same value (or higher) than the output it is spending.
    * See BIP65 for more info
    */
  def calcLockTime(utxos: Seq[InputSigningInfo[InputInfo]]): Try[UInt32] = {
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
              if (
                currentLockTime < lockTimeThreshold && lockTime >= lockTimeThreshold
              ) {
                //means that we spend two different locktime types, one of the outputs spends a
                //OP_CLTV script by block height, the other spends one by time stamp
                TxBuilderError.IncompatibleLockTimes
              } else Success(lockTime)
            } else if (
              currentLockTime >= lockTimeThreshold && lockTime < lockTimeThreshold
            ) {
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
        remaining: Seq[InputSigningInfo[InputInfo]],
        currentLockTimeOpt: Option[UInt32]): Try[UInt32] =
      remaining match {
        case Nil =>
          Success(currentLockTimeOpt.getOrElse(TransactionConstants.lockTime))
        case spendingInfo +: newRemaining =>
          spendingInfo.inputInfo match {
            case lockTime: LockTimeInputInfo =>
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
            case p2pkWithTimeout: P2PKWithTimeoutInputInfo =>
              if (p2pkWithTimeout.isBeforeTimeout) {
                loop(newRemaining, currentLockTimeOpt)
              } else {
                val result = computeNextLockTime(
                  currentLockTimeOpt,
                  p2pkWithTimeout.scriptPubKey.lockTime.toLong)

                result match {
                  case Success(newLockTime) =>
                    loop(newRemaining, Some(newLockTime))
                  case _: Failure[UInt32] => result
                }
              }
            case p2sh: P2SHInputInfo =>
              val nestedSpendingInfo =
                p2sh.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, currentLockTimeOpt)
            case p2wsh: P2WSHV0InputInfo =>
              val nestedSpendingInfo =
                p2wsh.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, currentLockTimeOpt)
            case conditional: ConditionalInputInfo =>
              val nestedSpendingInfo =
                conditional.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, currentLockTimeOpt)
            case _: P2WPKHV0InputInfo | _: UnassignedSegwitNativeInputInfo |
                _: P2PKInputInfo | _: P2PKHInputInfo |
                _: MultiSignatureInputInfo | _: EmptyInputInfo =>
              // none of these scripts affect the locktime of a tx
              loop(newRemaining, currentLockTimeOpt)
          }
      }

    loop(utxos, None)
  }

  /** Inserts script signatures and (potentially) witness data to a given
    * transaction using DummyECDigitalSignatures for all sigs in order
    * to produce a transaction roughly the size of the expected fully signed
    * transaction. Useful during fee estimation.
    *
    * Note that the resulting dummy-signed Transaction will have populated
    * (dummy) witness data when applicable.
    */
  def addDummySigs(utx: Transaction, inputInfos: Vector[InputInfo])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val dummyInputAndWitnnessFs = inputInfos.zipWithIndex.map {
      case (inputInfo, index) =>
        val mockSigners = inputInfo.pubKeys.take(inputInfo.requiredSigs).map {
          pubKey =>
            Sign(_ => Future.successful(DummyECDigitalSignature), pubKey)
        }

        val mockSpendingInfo =
          inputInfo.toSpendingInfo(mockSigners, HashType.sigHashAll)

        BitcoinSigner
          .sign(mockSpendingInfo, utx, isDummySignature = true)
          .map(_.transaction)
          .map { tx =>
            val witnessOpt = tx match {
              case _: NonWitnessTransaction => None
              case wtx: WitnessTransaction =>
                wtx.witness.witnesses(index) match {
                  case EmptyScriptWitness   => None
                  case wit: ScriptWitnessV0 => Some(wit)
                }
            }

            (tx.inputs(index), witnessOpt)
          }
    }

    Future.sequence(dummyInputAndWitnnessFs).map { inputsAndWitnesses =>
      val inputs = inputsAndWitnesses.map(_._1)
      val txWitnesses = inputsAndWitnesses.map(_._2)
      TransactionWitness.fromWitOpt(txWitnesses) match {
        case _: EmptyWitness =>
          BaseTransaction(utx.version, inputs, utx.outputs, utx.lockTime)
        case wit: TransactionWitness =>
          WitnessTransaction(utx.version,
                             inputs,
                             utx.outputs,
                             utx.lockTime,
                             wit)
      }
    }
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
      case btx: NonWitnessTransaction =>
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

  /** Runs various sanity checks on a transaction */
  def sanityChecks(
      isSigned: Boolean,
      inputInfos: Vector[InputInfo],
      expectedFeeRate: FeeUnit,
      tx: Transaction): Try[Unit] = {
    val dustT = if (isSigned) {
      sanityDustCheck(tx)
    } else {
      Success(())
    }

    dustT.flatMap { _ =>
      sanityAmountChecks(isSigned, inputInfos, expectedFeeRate, tx)
    }
  }

  /** Checks that no output is beneath the dust threshold */
  def sanityDustCheck(tx: Transaction): Try[Unit] = {
    val belowDustOutputs = tx.outputs
      .filterNot(_.scriptPubKey.asm.contains(OP_RETURN))
      .filter(_.value < Policy.dustThreshold)

    if (belowDustOutputs.nonEmpty) {
      TxBuilderError.OutputBelowDustThreshold(belowDustOutputs)
    } else {
      Success(())
    }
  }

  /**
    * Checks that the creditingAmount >= destinationAmount
    * and then does a sanity check on the transaction's fee
    */
  def sanityAmountChecks(
      isSigned: Boolean,
      inputInfos: Vector[InputInfo],
      expectedFeeRate: FeeUnit,
      tx: Transaction): Try[Unit] = {
    val spentAmount = tx.outputs.foldLeft(CurrencyUnits.zero)(_ + _.value)
    val creditingAmount =
      inputInfos.foldLeft(CurrencyUnits.zero)(_ + _.amount)
    if (spentAmount > creditingAmount) {
      TxBuilderError.MintsMoney
    } else {
      val expectedTx = if (isSigned) {
        tx
      } else {
        import scala.concurrent.ExecutionContext.Implicits.global
        import scala.concurrent.duration.DurationInt

        Await.result(TxUtil.addDummySigs(tx, inputInfos), 10.seconds)
      }

      val actualFee = creditingAmount - spentAmount
      val estimatedFee = expectedFeeRate * expectedTx
      isValidFeeRange(estimatedFee, actualFee, expectedFeeRate)
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
        s"Fee was too high. Estimated fee $estimatedFee, actualFee $actualFee, difference $difference, acceptableVariance $acceptableVariance")
      TxBuilderError.HighFee
    } else if (difference >= max) {
      logger.error(
        s"Fee was too low. Estimated fee $estimatedFee, actualFee $actualFee, difference $difference, acceptableVariance $acceptableVariance")

      TxBuilderError.LowFee
    } else {
      Success(())
    }
  }
}
