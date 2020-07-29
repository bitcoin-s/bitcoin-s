package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{InputInfo, InputSigningInfo}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Random, Success, Try}

/** This trait is responsible for converting RawTxBuilderResults into
  * finalized (unsigned) transactions. This process usually includes
  * such things as computation on inputs and outputs to generate
  * things like change outputs, or to reorder inputs or outputs.
  *
  * Once a transaction is done being finalized, its txid/wtxid should
  * not change as the RawTxSigner's only responsibility is adding signature
  * data not included in the txid/wtxid.
  *
  * RawTxFinalizer may (but is not required to) generate witness data
  * other than signatures (i.e. public keys for P2WPKH and redeem scripts
  * for P2WSH). RawTxFinalizer may not otherwise populate any other kind
  * of script signature or witness data.
  *
  * RawTxFinalizers are compose-able through the andThen method which will
  * turn the first RawTxFinalizer's finalized transaction into a RawTxBuilderResult
  * by taking that transactions inputs (in order), outputs (in order), locktime and
  * version and this RawTxBuilderResult is then given to the second RawTxFinalizer.
  */
trait RawTxFinalizer {

  /** Constructs a finalized (unsigned) transaction */
  def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction]

  /** The result of buildTx is converted into a RawTxBuilderResult
    * by taking that transactions inputs (in order), outputs (in order),
    * locktime and version and this RawTxBuilderResult is then passed to
    * the other RawTxFinalizer's buildTx
    */
  def andThen(other: RawTxFinalizer): RawTxFinalizer = {
    // this.buildTx above gets shadowed below, so this allows us to call it
    def thisBuildTx(txBuilderResult: RawTxBuilderResult)(implicit
        ec: ExecutionContext): Future[Transaction] =
      this.buildTx(txBuilderResult)

    new RawTxFinalizer {
      override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
          ec: ExecutionContext): Future[Transaction] = {
        for {
          firstFinalizedTx <- thisBuildTx(txBuilderResult)
          composedFinalizedTx <-
            other.buildTx(RawTxBuilderResult.fromTransaction(firstFinalizedTx))
        } yield composedFinalizedTx
      }
    }
  }
}

abstract class FinalizerFactory[T <: RawTxFinalizer] {

  def txBuilderFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): RawTxBuilderWithFinalizer[T] = {
    val inputs = InputUtil.calcSequenceForInputs(utxos)
    val lockTime = TxUtil.calcLockTime(utxos).get
    val builder = RawTxBuilder().setLockTime(lockTime) ++= outputs ++= inputs
    val finalizer =
      txFinalizerFrom(utxos.toVector.map(_.inputInfo), feeRate, changeSPK)

    builder.setFinalizer(finalizer)
  }

  def txFinalizerFrom(
      inputs: Vector[InputInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): T

  def txFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val builderF = Future(txBuilderFrom(outputs, utxos, feeRate, changeSPK))

    builderF.flatMap(_.buildTx())
  }
}

/** A trivial finalizer that does no processing */
case object RawFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    Future.successful(txBuilderResult.toBaseTransaction)
  }
}

/** A simple finalizer that only removes outputs beneath the dust
  * threshold and does nothing else
  */
case object FilterDustFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val filteredOutputs =
      txBuilderResult.outputs.filter(_.value >= Policy.dustThreshold)
    Future.successful(
      txBuilderResult.toBaseTransaction.copy(outputs = filteredOutputs))
  }
}

case object BIP69Finalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val sortedInputs = txBuilderResult.inputs.sorted
    val sortedOutputs = txBuilderResult.outputs.sorted
    Future.successful(
      txBuilderResult.toBaseTransaction.copy(inputs = sortedInputs,
                                             outputs = sortedOutputs))
  }
}

/** A finalizer who's Future fails if its sanity checks are not passed,
  * otherwise it does nothing.
  */
case class SanityCheckFinalizer(
    inputInfos: Vector[InputInfo],
    expectedOutputSPKs: Vector[ScriptPubKey],
    expectedFeeRate: FeeUnit,
    changeSPKs: Vector[ScriptPubKey] = Vector.empty)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val tx = txBuilderResult.toBaseTransaction

    val passInOutChecksT =
      SanityCheckFinalizer.sanityDestinationChecks(inputInfos.map(_.outPoint),
                                                   expectedOutputSPKs,
                                                   changeSPKs,
                                                   tx)

    val passChecksT = passInOutChecksT.flatMap { _ =>
      TxUtil.sanityChecks(isSigned = false, inputInfos, expectedFeeRate, tx)
    }

    Future.fromTry(passChecksT.map(_ => tx))
  }
}

object SanityCheckFinalizer {

  /** Checks that a finalized transaction contains the expected
    * inputs and scriptpubkeys.
    *
    * Note that it is not responsible for checking output values
    * or that change isn't dropped (as it can be dust). That is
    * covered by TxUtil.sanityChecks above
    */
  def sanityDestinationChecks(
      expectedOutPoints: Vector[TransactionOutPoint],
      expectedOutputSPKs: Vector[ScriptPubKey],
      changeSPKs: Vector[ScriptPubKey],
      finalizedTx: Transaction): Try[Unit] = {
    //make sure we send coins to the appropriate destinations
    val finalizedSPKs = finalizedTx.outputs.map(_.scriptPubKey)
    val isMissingDestination =
      !expectedOutputSPKs.forall(finalizedSPKs.contains)
    val hasExtraOutputs = finalizedSPKs
      .diff(expectedOutputSPKs)
      .diff(changeSPKs)
      .nonEmpty
    val spendingTxOutPoints = finalizedTx.inputs.map(_.previousOutput)
    val hasExtraOutPoints =
      !spendingTxOutPoints.forall(expectedOutPoints.contains) ||
        expectedOutPoints.length != spendingTxOutPoints.length
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
}

/** A finalizer which performs fee estimation and adds a
  * change output resulting in the expected fee.
  */
case class ChangeFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val outputsWithDummyChange =
      txBuilderResult.outputs :+ TransactionOutput(Satoshis.zero, changeSPK)

    val totalCrediting = inputInfos
      .map(_.output.value)
      .foldLeft[CurrencyUnit](Satoshis.zero)(_ + _)
    val totalSpending =
      txBuilderResult.outputs
        .map(_.value)
        .foldLeft[CurrencyUnit](Satoshis.zero)(_ + _)

    val txDummyChange =
      txBuilderResult.toBaseTransaction.copy(outputs = outputsWithDummyChange)
    val dummyTxF = TxUtil.addDummySigs(txDummyChange, inputInfos)

    dummyTxF.map { dummyTx =>
      val fee = feeRate.calc(dummyTx)
      val change = totalCrediting - totalSpending - fee

      val newChangeOutput = TransactionOutput(change, changeSPK)

      val newOutputs = if (change <= Policy.dustThreshold) {
        txBuilderResult.outputs
      } else {
        txBuilderResult.outputs :+ newChangeOutput
      }

      txBuilderResult.toBaseTransaction.copy(outputs = newOutputs)
    }
  }
}

/** A finalizer which adds only the witness data included in the
  * wtxid (pubkey in P2WPKH and redeem script in p2sh).
  *
  * Note that when used in composition with other finalizers,
  * if AddWitnessDataFinalizer is not last then its effects
  * will be reversed since witness data is not currently kept
  * between finalizers during composition.
  */
case class AddWitnessDataFinalizer(inputInfos: Vector[InputInfo])
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val witnesses = inputInfos.map(InputInfo.getScriptWitness)
    TransactionWitness.fromWitOpt(witnesses) match {
      case _: EmptyWitness =>
        Future.successful(txBuilderResult.toBaseTransaction)
      case wit: TransactionWitness =>
        val wtx =
          WitnessTransaction
            .toWitnessTx(txBuilderResult.toBaseTransaction)
            .copy(witness = wit)

        Future.successful(wtx)
    }
  }
}

/** A finalizer which adds a change output, performs sanity checks,
  * and adds non-signature witness data. This is the standard
  * non-interactive finalizer within the Bitcoin-S wallet.
  */
case class StandardNonInteractiveFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val addChange = ChangeFinalizer(inputInfos, feeRate, changeSPK)

    val sanityCheck = SanityCheckFinalizer(
      inputInfos = inputInfos,
      expectedOutputSPKs = txBuilderResult.outputs.map(_.scriptPubKey),
      expectedFeeRate = feeRate,
      changeSPKs = Vector(changeSPK))

    val addWitnessData = AddWitnessDataFinalizer(inputInfos)

    addChange
      .andThen(sanityCheck)
      .andThen(addWitnessData)
      .buildTx(txBuilderResult)
  }
}

object StandardNonInteractiveFinalizer
    extends FinalizerFactory[StandardNonInteractiveFinalizer] {

  override def txFinalizerFrom(
      inputs: Vector[InputInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): StandardNonInteractiveFinalizer = {
    StandardNonInteractiveFinalizer(inputs, feeRate, changeSPK)
  }
}

/** A finalizer which adds a change output, performs sanity checks,
  * shuffles inputs and outputs, and adds non-signature witness data.
  * This is the standard non-interactive finalizer within the Bitcoin-S wallet.
  */
case class ShufflingNonInteractiveFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val addChange = ChangeFinalizer(inputInfos, feeRate, changeSPK)

    val shuffled = addChange.andThen(ShuffleFinalizer)

    shuffled.buildTx(txBuilderResult).flatMap { tempTx =>
      val tempTxBuilderResult = RawTxBuilderResult.fromTransaction(tempTx)

      val shuffledInputInfos = tempTxBuilderResult.inputs
        .map(input => inputInfos.find(_.outPoint == input.previousOutput).get)

      val sanityCheck =
        SanityCheckFinalizer(inputInfos = shuffledInputInfos,
                             expectedOutputSPKs =
                               tempTxBuilderResult.outputs.map(_.scriptPubKey),
                             expectedFeeRate = feeRate,
                             changeSPKs = Vector(changeSPK))

      sanityCheck
        .andThen(AddWitnessDataFinalizer(shuffledInputInfos))
        .buildTx(tempTxBuilderResult)
    }
  }
}

object ShufflingNonInteractiveFinalizer
    extends FinalizerFactory[ShufflingNonInteractiveFinalizer] {

  override def txFinalizerFrom(
      inputs: Vector[InputInfo],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): ShufflingNonInteractiveFinalizer = {
    ShufflingNonInteractiveFinalizer(inputs, feeRate, changeSPK)
  }
}

case class SubtractFeeFromOutputsFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val RawTxBuilderResult(version, inputs, outputs, lockTime) = txBuilderResult

    val witnesses = inputInfos.map(InputInfo.getScriptWitness)
    val txWithPossibleWitness = TransactionWitness.fromWitOpt(witnesses) match {
      case _: EmptyWitness =>
        BaseTransaction(version, inputs, outputs, lockTime)
      case wit: TransactionWitness =>
        WitnessTransaction(version, inputs, outputs, lockTime, wit)
    }

    val dummyTxF = TxUtil.addDummySigs(txWithPossibleWitness, inputInfos)

    val outputsAfterFeeF = dummyTxF.map { dummyTx =>
      SubtractFeeFromOutputsFinalizer.subtractFees(dummyTx,
                                                   feeRate,
                                                   outputs.map(_.scriptPubKey))
    }

    outputsAfterFeeF.map { outputsAfterFee =>
      BaseTransaction(version, inputs, outputsAfterFee, lockTime)
    }
  }
}

object SubtractFeeFromOutputsFinalizer {

  def subtractFees(
      tx: Transaction,
      feeRate: FeeUnit,
      spks: Vector[ScriptPubKey]): Vector[TransactionOutput] = {
    val fee = feeRate.calc(tx)

    val outputs = tx.outputs.zipWithIndex.filter {
      case (output, _) => spks.contains(output.scriptPubKey)
    }
    val unchangedOutputs = tx.outputs.zipWithIndex.filterNot {
      case (output, _) => spks.contains(output.scriptPubKey)
    }

    val feePerOutput = Satoshis(Int64(fee.satoshis.toLong / outputs.length))
    val feeRemainder = Satoshis(Int64(fee.satoshis.toLong % outputs.length))

    val newOutputsWithoutRemainder = outputs.map {
      case (output, index) =>
        (TransactionOutput(output.value - feePerOutput, output.scriptPubKey),
         index)
    }
    val (lastOutput, lastOutputIndex) = newOutputsWithoutRemainder.last
    val newLastOutput = TransactionOutput(lastOutput.value - feeRemainder,
                                          lastOutput.scriptPubKey)
    val newOutputs = newOutputsWithoutRemainder
      .dropRight(1)
      .:+((newLastOutput, lastOutputIndex))

    (newOutputs ++ unchangedOutputs).sortBy(_._2).map(_._1).toVector
  }
}

/** Shuffles in the inputs and outputs of the Transaction into a random order */
case object ShuffleFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    ShuffleInputsFinalizer
      .andThen(ShuffleOutputsFinalizer)
      .buildTx(txBuilderResult)
  }
}

/** Shuffles in the inputs of the Transaction into a random order */
case object ShuffleInputsFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val shuffledInputs = Random.shuffle(txBuilderResult.inputs)
    Future.successful(
      txBuilderResult.toBaseTransaction.copy(inputs = shuffledInputs))
  }
}

/** Shuffles in the outputs of the Transaction into a random order */
case object ShuffleOutputsFinalizer extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val shuffledOutputs = Random.shuffle(txBuilderResult.outputs)
    Future.successful(
      txBuilderResult.toBaseTransaction.copy(outputs = shuffledOutputs))
  }
}
