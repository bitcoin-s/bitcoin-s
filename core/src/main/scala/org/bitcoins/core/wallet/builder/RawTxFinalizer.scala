package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{InputInfo, InputSigningInfo}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

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
sealed trait RawTxFinalizer {

  /** Constructs a finalized (unsigned) transaction */
  def buildTx(txBuilderResult: RawTxBuilderResult)(
      implicit ec: ExecutionContext): Future[Transaction]

  /** The result of buildTx is converted into a RawTxBuilderResult
    * by taking that transactions inputs (in order), outputs (in order),
    * locktime and version and this RawTxBuilderResult is then passed to
    * the other RawTxFinalizer's buildTx
    */
  def andThen(other: RawTxFinalizer): RawTxFinalizer = new RawTxFinalizer {
    override def buildTx(txBuilderResult: RawTxBuilderResult)(
        implicit ec: ExecutionContext): Future[Transaction] = {
      for {
        firstFinalizedTx <- this.buildTx(txBuilderResult)
        composedFinalizedTx <- other.buildTx(
          RawTxBuilderResult.fromTransaction(firstFinalizedTx))
      } yield composedFinalizedTx
    }
  }
}

/** A trivial finalizer that does no processing */
case object RawFinalizer extends RawTxFinalizer {
  override def buildTx(txBuilderResult: RawTxBuilderResult)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    Future.successful(txBuilderResult.toBaseTransaction)
  }
}

/** A simple finalizer that only removes outputs beneath the dust
  * threshold and does nothing else
  */
case object FilterDustFinalizer extends RawTxFinalizer {
  override def buildTx(txBuilderResult: RawTxBuilderResult)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val filteredOutputs =
      txBuilderResult.outputs.filter(_.value >= Policy.dustThreshold)
    Future.successful(
      txBuilderResult.toBaseTransaction.copy(outputs = filteredOutputs))
  }
}

/** A finalizer which takes as parameters an input info for each input, as well
  * as a fee rate and change scriptpubkey and adds a change output resulting in
  * the expected fee (after fee estimation).
  */
case class NonInteractiveWithChangeFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey)
    extends RawTxFinalizer {
  override def buildTx(txBuilderResult: RawTxBuilderResult)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val RawTxBuilderResult(version, inputs, outputs, lockTime) = txBuilderResult

    val outputsWithChange = outputs :+ TransactionOutput(Satoshis.zero,
                                                         changeSPK)
    val totalCrediting = inputInfos
      .map(_.output.value)
      .foldLeft[CurrencyUnit](Satoshis.zero)(_ + _)
    val totalSpending =
      outputs.map(_.value).foldLeft[CurrencyUnit](Satoshis.zero)(_ + _)
    val witnesses = inputInfos.map(InputInfo.getScriptWitness)
    val txNoChangeFee = TransactionWitness.fromWitOpt(witnesses) match {
      case _: EmptyWitness =>
        BaseTransaction(version, inputs, outputsWithChange, lockTime)
      case wit: TransactionWitness =>
        WitnessTransaction(version, inputs, outputsWithChange, lockTime, wit)
    }

    val dummyTxF = TxUtil.addDummySigs(txNoChangeFee, inputInfos)

    val txF = dummyTxF.map { dummyTx =>
      val fee = feeRate.calc(dummyTx)
      val change = totalCrediting - totalSpending - fee
      val newChangeOutput = TransactionOutput(change, changeSPK)
      val newOutputs = if (change <= Policy.dustThreshold) {
        outputs
      } else {
        outputs :+ newChangeOutput
      }

      txNoChangeFee match {
        case btx: NonWitnessTransaction =>
          BaseTransaction(btx.version, btx.inputs, newOutputs, btx.lockTime)
        case WitnessTransaction(version, inputs, _, lockTime, witness) =>
          WitnessTransaction(version, inputs, newOutputs, lockTime, witness)
      }
    }

    txF.flatMap { tx =>
      val passInOutChecksT =
        NonInteractiveWithChangeFinalizer.sanityDestinationChecks(
          expectedOutPoints = inputs.map(_.previousOutput),
          expectedOutputs = outputs,
          changeSPK = changeSPK,
          finalizedTx = tx)

      val passChecksT = passInOutChecksT.flatMap { _ =>
        TxUtil.sanityChecks(isSigned = false,
                            inputInfos = inputInfos,
                            expectedFeeRate = feeRate,
                            tx = tx)
      }

      Future.fromTry(passChecksT.map(_ => tx))
    }
  }
}

object NonInteractiveWithChangeFinalizer {

  /** Checks that a finalized transaction contains the expected
    * inputs and outputs. */
  def sanityDestinationChecks(
      expectedOutPoints: Vector[TransactionOutPoint],
      expectedOutputs: Vector[TransactionOutput],
      changeSPK: ScriptPubKey,
      finalizedTx: Transaction): Try[Unit] = {
    //make sure we send coins to the appropriate destinations
    val isMissingDestination =
      !expectedOutputs.forall(finalizedTx.outputs.contains)
    val hasExtraOutputs =
      if (finalizedTx.outputs.size == expectedOutputs.size) {
        false
      } else {
        //the extra output should be the changeOutput
        !(finalizedTx.outputs.size == (expectedOutputs.size + 1) &&
          finalizedTx.outputs.map(_.scriptPubKey).contains(changeSPK))
      }
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

  def txBuilderFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey): RawTxBuilderWithFinalizer = {
    val inputs = InputUtil.calcSequenceForInputs(utxos, Policy.isRBFEnabled)
    val lockTime = TxUtil.calcLockTime(utxos).get
    val builder = RawTxBuilder().setLockTime(lockTime) ++= outputs ++= inputs
    val finalizer = NonInteractiveWithChangeFinalizer(
      utxos.toVector.map(_.inputInfo),
      feeRate,
      changeSPK)

    builder.setFinalizer(finalizer)
  }

  def txFrom(
      outputs: Seq[TransactionOutput],
      utxos: Seq[InputSigningInfo[InputInfo]],
      feeRate: FeeUnit,
      changeSPK: ScriptPubKey)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    val builderF = Future(txBuilderFrom(outputs, utxos, feeRate, changeSPK))

    builderF.flatMap(_.buildTx())
  }
}
