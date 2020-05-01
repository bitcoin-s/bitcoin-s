package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.Int64
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{P2WPKHWitnessV0, ScriptPubKey}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{InputInfo, InputSigningInfo}
import org.bitcoins.crypto.{DummyECDigitalSignature, ECPublicKey}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

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

object StandardNonInteractiveFinalizer {

  def txBuilderFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): RawTxBuilderWithFinalizer[
    StandardNonInteractiveFinalizer] = {
    val inputs = InputUtil.calcSequenceForInputs(utxos)
    val lockTime = TxUtil.calcLockTime(utxos).get
    val builder = RawTxBuilder().setLockTime(lockTime) ++= outputs ++= inputs
    val finalizer = StandardNonInteractiveFinalizer(
      utxos.toVector.map(_.inputInfo),
      feeRate,
      changeSPK)

    builder.setFinalizer(finalizer)
  }

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

/** Adds a future fee amount to the output with the given
  * ScriptPubKey and subtracts that amount in equal portions
  * from the specified change ScriptPubKeys.
  */
case class AddFutureFeeFinalizer(
    spk: ScriptPubKey,
    futureFee: CurrencyUnit,
    changeSPKs: Vector[ScriptPubKey])
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val changeOutputs = txBuilderResult.outputs.filter(output =>
      changeSPKs.contains(output.scriptPubKey))

    val outputT = txBuilderResult.outputs.zipWithIndex
      .find(_._1.scriptPubKey == spk) match {
      case Some((output, index)) =>
        val newOutput = output.copy(value = output.value + futureFee)
        Success((newOutput, index))
      case None =>
        Failure(new RuntimeException(
          s"Did not find expected SPK $spk in ${txBuilderResult.outputs.map(_.scriptPubKey)}"))
    }

    val outputsT = outputT.map {
      case (newOutput, index) =>
        val subFromChange =
          Satoshis(futureFee.satoshis.toLong / changeOutputs.length)
        val outputsMinusChange = txBuilderResult.outputs.map { output =>
          if (changeSPKs.contains(output.scriptPubKey)) {
            output.copy(value = output.value - subFromChange)
          } else {
            output
          }
        }

        outputsMinusChange.updated(index, newOutput)
    }

    val txT = outputsT.map { outputs =>
      txBuilderResult.toBaseTransaction.copy(outputs = outputs)
    }

    Future.fromTry(txT)
  }
}

/** Assumes the input transaction has had no fee considerations
  * and subtracts the estimated fee in equal portions from the
  * outputs with the specified ScriptPubKeys
  */
case class SubtractFeeFromOutputsFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    spks: Vector[ScriptPubKey])
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val dummyTxF =
      TxUtil.addDummySigs(txBuilderResult.toBaseTransaction, inputInfos)

    val outputsAfterFeeF = dummyTxF.map { dummyTx =>
      SubtractFeeFromOutputsFinalizer.subtractFees(
        dummyTx,
        feeRate,
        spks
      )
    }

    outputsAfterFeeF.map { outputsAfterFee =>
      txBuilderResult.toBaseTransaction.copy(outputs = outputsAfterFee)
    }
  }
}

object SubtractFeeFromOutputsFinalizer {

  def subtractFees(
      tx: Transaction,
      feeRate: FeeUnit,
      spks: Vector[ScriptPubKey]): Vector[TransactionOutput] = {
    val fee = feeRate.calc(tx)

    val (outputs, unchangedOutputs) =
      tx.outputs.zipWithIndex.toVector.partition {
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

    (newOutputs ++ unchangedOutputs).sortBy(_._2).map(_._1)
  }
}

/** Assumes the input transaction has had no fee considerations
  * and that all inputs are P2WPKH. Subtracts the estimated fee
  * in equal portions from the outputs with the specified ScriptPubKeys
  */
case class P2WPKHSubtractFeesFromOutputsFinalizer(
    feeRate: FeeUnit,
    spks: Vector[ScriptPubKey])
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val dummyTxWit = TransactionWitness(
      Vector.fill(txBuilderResult.inputs.length)(
        P2WPKHWitnessV0(ECPublicKey.freshPublicKey, DummyECDigitalSignature)))

    val wtx = txBuilderResult.toWitnessTransaction(dummyTxWit)

    val outputsAfterFee =
      SubtractFeeFromOutputsFinalizer.subtractFees(wtx, feeRate, spks)

    Future.successful(
      txBuilderResult.toBaseTransaction.copy(outputs = outputsAfterFee))
  }
}

/** Finalizes a dual-funded transaction given the InputInfos
  * from both parties and the non-change output ScriptPubKey.
  *
  * This includes adding the future fee of spending transactions
  * to the non-change output as well as subtracting relevant fees
  * from the change outputs. This finalizer filters dust outputs.
  *
  * Note: spendingVBytes may be 0
  */
case class DualFundingTxFinalizer(
    spendingVBytes: Long,
    feeRate: FeeUnit,
    fundingSPK: ScriptPubKey,
    inputInfos: Vector[InputInfo])
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val changeSPKs =
      txBuilderResult.outputs.map(_.scriptPubKey).filterNot(_ == fundingSPK)

    val addFutureFee = AddFutureFeeFinalizer(
      fundingSPK,
      Satoshis(feeRate.toLong * spendingVBytes),
      changeSPKs)
    val subtractFee =
      SubtractFeeFromOutputsFinalizer(inputInfos, feeRate, changeSPKs)
    val filterDust = FilterDustFinalizer

    addFutureFee
      .andThen(subtractFee)
      .andThen(filterDust)
      .buildTx(txBuilderResult)
  }
}

/** Finalizes a dual-funded transaction given the non-change output
  * ScriptPubKey and under the assumption that all funding inputs
  * from both parties are P2WPKH UTXOs.
  *
  * This includes adding the future fee of spending transactions
  * to the non-change output as well as subtracting relevant fees
  * from the change outputs. This finalizer filters dust outputs.
  *
  * Note: spendingVBytes may be 0
  */
case class P2WPKHDualFundingTxFinalizer(
    spendingVBytes: Long,
    feeRate: FeeUnit,
    fundingSPK: ScriptPubKey)
    extends RawTxFinalizer {

  override def buildTx(txBuilderResult: RawTxBuilderResult)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    val changeSPKs =
      txBuilderResult.outputs.map(_.scriptPubKey).filterNot(_ == fundingSPK)

    val addFutureFee = AddFutureFeeFinalizer(
      fundingSPK,
      Satoshis(feeRate.toLong * spendingVBytes),
      changeSPKs)
    val subtractFee =
      P2WPKHSubtractFeesFromOutputsFinalizer(feeRate, changeSPKs)
    val filterDust = FilterDustFinalizer

    addFutureFee
      .andThen(subtractFee)
      .andThen(filterDust)
      .buildTx(txBuilderResult)
  }
}
