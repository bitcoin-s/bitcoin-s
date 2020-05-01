package org.bitcoins.core.wallet.builder

import org.bitcoins.core.crypto.TxSigComponent
import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.protocol.transaction.{
  EmptyWitness,
  Transaction,
  TransactionWitness,
  WitnessTransaction
}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo.{
  InputInfo,
  ScriptSignatureParams,
  UnassignedSegwitNativeInputInfo
}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object RawTxSigner extends BitcoinSLogger {

  def sign(txWithInfo: FinalizedTxWithSigningInfo, expectedFeeRate: FeeUnit)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    sign(txWithInfo.finalizedTx, txWithInfo.infos, expectedFeeRate)
  }

  def sign(
      utx: Transaction,
      utxoInfos: Vector[ScriptSignatureParams[InputInfo]],
      expectedFeeRate: FeeUnit)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    sign(utx, utxoInfos, expectedFeeRate, (_, _) => true)
  }

  def sign(
      txWithInfo: FinalizedTxWithSigningInfo,
      expectedFeeRate: FeeUnit,
      invariants: (
          Seq[ScriptSignatureParams[InputInfo]],
          Transaction) => Boolean)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    sign(txWithInfo.finalizedTx, txWithInfo.infos, expectedFeeRate, invariants)
  }

  def sign(
      utx: Transaction,
      utxoInfos: Vector[ScriptSignatureParams[InputInfo]],
      expectedFeeRate: FeeUnit,
      invariants: (
          Seq[ScriptSignatureParams[InputInfo]],
          Transaction) => Boolean)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    require(utxoInfos.length == utx.inputs.length,
            "Must provide exactly one UTXOSatisfyingInfo per input.")
    require(utxoInfos.distinct.length == utxoInfos.length,
            "All UTXOSatisfyingInfos must be unique. ")
    require(utxoInfos.forall(utxo =>
              utx.inputs.exists(_.previousOutput == utxo.outPoint)),
            "All UTXOSatisfyingInfos must correspond to an input.")

    val signedTxF =
      if (utxoInfos.exists(
            _.inputInfo.isInstanceOf[UnassignedSegwitNativeInputInfo])) {
        Future.fromTry(TxBuilderError.NoSigner)
      } else {
        val builder = RawTxBuilder()
          .setVersion(utx.version)
          .setLockTime(utx.lockTime) ++= utx.outputs

        val inputAndWitnessFs = utxoInfos.map { utxo =>
          val txSigCompF =
            BitcoinSigner.sign(utxo, utx, isDummySignature = false)
          txSigCompF.map { txSigComp =>
            val scriptWitnessOpt = TxSigComponent.getScriptWitness(txSigComp)

            if (scriptWitnessOpt.isEmpty && InputInfo
                  .getScriptWitness(utxo.inputInfo)
                  .isDefined) {
              println(utxo.inputInfo)
            }

            (txSigComp.input, scriptWitnessOpt)
          }
        }

        val witnessesBuilder = Vector.newBuilder[Option[ScriptWitness]]

        val inputsAddedToBuilderF = Future.sequence(inputAndWitnessFs).map {
          inputsAndWitnesses =>
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
          btx <- builder.result(RawFinalizer)
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
          //final sanity checks
          Transaction.sanityChecks(forSigned = true,
                                   inputInfos = utxoInfos.map(_.inputInfo),
                                   expectedFeeRate = expectedFeeRate,
                                   tx = signedTx) match {
            case Success(_)   => Success(signedTx)
            case Failure(err) => Failure(err)
          }
        } else {
          TxBuilderError.FailedUserInvariants
        }
      }

      Future.fromTry(txT)
    }
  }
}
