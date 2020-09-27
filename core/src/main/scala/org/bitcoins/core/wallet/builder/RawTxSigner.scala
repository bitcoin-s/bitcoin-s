package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.TxSigComponent
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo.{
  InputInfo,
  ScriptSignatureParams,
  UnassignedSegwitNativeInputInfo
}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Try}

/** Transactions that have been finalized by a RawTxFinalizer are passed as inputs
  * to a sign function here in order to generate fully signed transactions.
  *
  * In the future sign methods specific to multi-party protocols will be added
  * here to support PSBT and signed transaction construction between multiple parties.
  */
object RawTxSigner extends BitcoinSLogger {

  val emptyInvariant: (
      Vector[ScriptSignatureParams[InputInfo]],
      Transaction) => Boolean = (_, _) => true

  def feeInvariant(expectedFeeRate: FeeUnit): (
      Vector[ScriptSignatureParams[InputInfo]],
      Transaction) => Boolean =
    addFeeRateInvariant(expectedFeeRate, emptyInvariant)

  private def addFeeRateInvariant(
      expectedFeeRate: FeeUnit,
      userInvariants: (
          Vector[ScriptSignatureParams[InputInfo]],
          Transaction) => Boolean): (
      Vector[ScriptSignatureParams[InputInfo]],
      Transaction) => Boolean = { (utxoInfos, signedTx) =>
    {
      userInvariants(utxoInfos, signedTx) &&
      TxUtil
        .sanityChecks(isSigned = true,
                      inputInfos = utxoInfos.map(_.inputInfo),
                      expectedFeeRate = expectedFeeRate,
                      tx = signedTx)
        .isSuccess
    }
  }

  def sign(
      utx: Transaction,
      utxoInfos: Vector[ScriptSignatureParams[InputInfo]])(implicit
      ec: ExecutionContext): Future[Transaction] = {
    sign(utx, utxoInfos, emptyInvariant, dummySign = false)
  }

  def sign(txWithInfo: FinalizedTxWithSigningInfo, expectedFeeRate: FeeUnit)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    sign(txWithInfo.finalizedTx, txWithInfo.infos, expectedFeeRate)
  }

  def sign(
      utx: Transaction,
      utxoInfos: Vector[ScriptSignatureParams[InputInfo]],
      expectedFeeRate: FeeUnit)(implicit
      ec: ExecutionContext): Future[Transaction] = {

    val invariants = feeInvariant(expectedFeeRate)

    sign(utx, utxoInfos, invariants, dummySign = false)
  }

  def sign(
      utx: Transaction,
      utxoInfos: Vector[ScriptSignatureParams[InputInfo]],
      expectedFeeRate: FeeUnit,
      userInvariants: (
          Vector[ScriptSignatureParams[InputInfo]],
          Transaction) => Boolean)(implicit
      ec: ExecutionContext): Future[Transaction] = {

    val invariants = addFeeRateInvariant(expectedFeeRate, userInvariants)

    sign(utx, utxoInfos, invariants, dummySign = false)
  }

  def sign(
      txWithInfo: FinalizedTxWithSigningInfo,
      expectedFeeRate: FeeUnit,
      userInvariants: (
          Vector[ScriptSignatureParams[InputInfo]],
          Transaction) => Boolean)(implicit
      ec: ExecutionContext): Future[Transaction] = {

    val invariants = addFeeRateInvariant(expectedFeeRate, userInvariants)

    sign(txWithInfo.finalizedTx,
         txWithInfo.infos,
         invariants,
         dummySign = false)
  }

  def sign(
      utx: Transaction,
      utxoInfos: Vector[ScriptSignatureParams[InputInfo]],
      invariants: (
          Vector[ScriptSignatureParams[InputInfo]],
          Transaction) => Boolean,
      dummySign: Boolean)(implicit
      ec: ExecutionContext): Future[Transaction] = {
    require(
      utxoInfos.length == utx.inputs.length,
      s"Must provide exactly one UTXOSatisfyingInfo per input, ${utxoInfos.length} != ${utx.inputs.length}")
    require(utxoInfos.distinct.length == utxoInfos.length,
            "All UTXOSatisfyingInfos must be unique. ")
    require(utxoInfos.forall(utxo =>
              utx.inputs.exists(_.previousOutput == utxo.outPoint)),
            "All UTXOSatisfyingInfos must correspond to an input.")

    val signedTxF =
      if (
        utxoInfos.exists(
          _.inputInfo.isInstanceOf[UnassignedSegwitNativeInputInfo])
      ) {
        Future.fromTry(TxBuilderError.NoSigner)
      } else {
        val builder = RawTxBuilder()
          .setVersion(utx.version)
          .setLockTime(utx.lockTime) ++= utx.outputs

        val inputAndWitnessFs = utxoInfos.map { utxo =>
          val txSigCompF =
            BitcoinSigner.sign(utxo, utx, isDummySignature = dummySign)
          txSigCompF.map { txSigComp =>
            val scriptWitnessOpt = TxSigComponent.getScriptWitness(txSigComp)

            (txSigComp.input, scriptWitnessOpt)
          }
        }

        val witnessesBuilder = Vector.newBuilder[Option[ScriptWitness]]

        val inputsAddedToBuilderF =
          Future.sequence(inputAndWitnessFs).map { inputsAndWitnesses =>
            utx.inputs.foreach { unsignedInput =>
              val (input, witnessOpt) = inputsAndWitnesses
                .find(_._1.previousOutput == unsignedInput.previousOutput)
                .get

              witnessesBuilder += witnessOpt
              builder += input
            }
          }

        for {
          _ <- inputsAddedToBuilderF
          btx <- builder.setFinalizer(RawFinalizer).buildTx()
        } yield {
          val txWitness =
            TransactionWitness.fromWitOpt(witnessesBuilder.result())

          txWitness match {
            case EmptyWitness(_) => btx
            case _: TransactionWitness =>
              WitnessTransaction(btx.version,
                                 btx.inputs,
                                 btx.outputs,
                                 btx.lockTime,
                                 txWitness)
          }
        }
      }

    signedTxF.flatMap { signedTx =>
      val txT: Try[Transaction] = {
        if (invariants(utxoInfos, signedTx)) {
          Success(signedTx)
        } else {
          TxBuilderError.FailedUserInvariants
        }
      }

      Future.fromTry(txT)
    }
  }
}
