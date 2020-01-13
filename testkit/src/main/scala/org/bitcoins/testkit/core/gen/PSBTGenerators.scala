package org.bitcoins.testkit.core.gen

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.psbt.{
  GlobalPSBTMap,
  GlobalPSBTRecord,
  InputPSBTMap,
  OutputPSBTMap,
  PSBT
}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.scalacheck.Gen

import scala.concurrent.{ExecutionContext, Future}

object PSBTGenerators {

  def psbtAndBuilderFromInputs(
      finalized: Boolean,
      creditingTxsInfo: Seq[BitcoinUTXOSpendingInfo],
      destinations: Seq[TransactionOutput],
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork,
      fee: FeeUnit)(
      implicit ec: ExecutionContext): Future[(PSBT, BitcoinTxBuilder)] = {
    val builderF =
      BitcoinTxBuilder(destinations, creditingTxsInfo, fee, changeSPK, network)
    for {
      builder <- builderF
      unsignedTx <- builder.unsignedTx

      orderedTxInfos = PSBT.SpendingInfoAndNonWitnessTxs
        .fromUnsignedTxAndInputs(unsignedTx, creditingTxsInfo.toVector)

      psbt <- {
        if (finalized) {
          PSBT.finalizedFromUnsignedTxAndInputs(unsignedTx, orderedTxInfos)
        } else {
          PSBT.fromUnsignedTxAndInputs(unsignedTx, orderedTxInfos)
        }
      }
    } yield (psbt, builder)
  }

  def psbtWithBuilder(finalized: Boolean)(
      implicit ec: ExecutionContext): Gen[Future[(PSBT, BitcoinTxBuilder)]] = {
    for {
      (creditingTxsInfo, destinations) <- CreditingTxGen.inputsAndOuptuts
      changeSPK <- ScriptGenerators.scriptPubKey
      network <- ChainParamsGenerator.bitcoinNetworkParams
      maxFee = {
        val crediting =
          creditingTxsInfo.foldLeft(0L)(_ + _.amount.satoshis.toLong)
        val spending = destinations.foldLeft(0L)(_ + _.value.satoshis.toLong)
        crediting - spending
      }
      fee <- CurrencyUnitGenerator.feeUnit(maxFee)
    } yield {
      psbtAndBuilderFromInputs(finalized = finalized,
                               creditingTxsInfo = creditingTxsInfo,
                               destinations = destinations,
                               changeSPK = changeSPK._1,
                               network = network,
                               fee = fee)
    }
  }

  def finalizedPSBTWithBuilder(
      implicit ec: ExecutionContext): Gen[Future[(PSBT, BitcoinTxBuilder)]] = {
    psbtWithBuilder(finalized = true)
  }

  def finalizedPSBT(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    finalizedPSBTWithBuilder.map(_.map(_._1))
  }

  def fullNonFinalizedPSBT(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    psbtWithBuilder(finalized = false).map(_.map(_._1))
  }

  def arbitraryPSBT(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    Gen.frequency((6, fullNonFinalizedPSBT), (1, finalizedPSBT)).map { psbtF =>
      psbtF.map { psbt =>
        val global = psbt.globalMap.elements
        val inputs = psbt.inputMaps
        val outputs = psbt.outputMaps

        def pruneVec[T](vec: Vector[T]): Vector[T] = {
          if (vec.isEmpty) {
            vec
          } else {
            val numKeep = scala.util.Random.nextInt(vec.length)
            vec
              .sortBy(_ => scala.util.Random.nextDouble())(
                Ordering.Double.TotalOrdering)
              .take(numKeep)
          }
        }

        val newGlobalElements = pruneVec(global) :+ GlobalPSBTRecord
          .UnsignedTransaction(psbt.transaction)
        val newGlobal = GlobalPSBTMap(newGlobalElements.distinct)
        val newInputs =
          inputs.map(input => InputPSBTMap(pruneVec(input.elements)))
        val newOutputs =
          outputs.map(output => OutputPSBTMap(pruneVec(output.elements)))

        PSBT(newGlobal, newInputs, newOutputs)
      }
    }
  }
}
