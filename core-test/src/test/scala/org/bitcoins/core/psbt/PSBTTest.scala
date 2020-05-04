package org.bitcoins.core.psbt

import org.bitcoins.core.psbt.OutputPSBTRecord.{RedeemScript, WitnessScript}
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.testkit.core.gen._
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Future
import scala.util.{Failure, Success}

class PSBTTest extends BitcoinSAsyncTest {

  behavior of "PSBT"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it must "correctly combine PSBTs" in {
    forAllAsync(PSBTGenerators.arbitraryPSBT) { psbtF =>
      psbtF.map { psbt =>
        val global = psbt.globalMap
        val inputs = psbt.inputMaps
        val outputs = psbt.outputMaps

        val newGlobal = PSBTGenerators.pruneGlobal(global)
        val newInputs =
          inputs.map(input =>
            InputPSBTMap(PSBTGenerators.pruneVec(input.elements)))
        val newOutputs =
          outputs.map(output =>
            OutputPSBTMap(PSBTGenerators.pruneVec(output.elements)))

        val psbt1 = PSBT(newGlobal, newInputs, newOutputs)

        val oppositeGlobalElements = global.elements.filterNot(e =>
          newGlobal.elements.contains(e)) :+ global.unsignedTransaction
        val oppositeGlobal = GlobalPSBTMap(oppositeGlobalElements.distinct)
        val oppositeInputs = inputs.zip(newInputs).map {
          case (map, pruned) =>
            InputPSBTMap(
              map.elements.filterNot(e => pruned.elements.contains(e)))
        }
        val oppositeOutputs = outputs.zip(newOutputs).map {
          case (map, pruned) =>
            OutputPSBTMap(
              map.elements.filterNot(e => pruned.elements.contains(e)))
        }

        val psbt2 = PSBT(oppositeGlobal, oppositeInputs, oppositeOutputs)

        assert(psbt1.combinePSBT(psbt2) == psbt)
        assert(psbt2.combinePSBT(psbt1) == psbt)
      }
    }
  }

  it must "correctly update PSBTs' inputs" in {
    forAllAsync(PSBTGenerators.psbtToBeSigned)(_.flatMap {
      case (fullPsbt, utxos) =>
        val emptyPsbt = PSBT.fromUnsignedTx(fullPsbt.transaction)

        val infoAndTxs = PSBTGenerators
          .spendingInfoAndNonWitnessTxsFromSpendingInfos(fullPsbt.transaction,
                                                         utxos.toVector)
          .infoAndTxOpts
        val updatedPSBT = infoAndTxs.zipWithIndex.foldLeft(emptyPsbt) {
          case (psbt, ((utxo, txOpt), index)) =>
            val partUpdatedPsbt = psbt
              .addUTXOToInput(txOpt.get, index)
              .addSigHashTypeToInput(utxo.hashType, index)

            (InputInfo.getRedeemScript(utxo.inputInfo),
             InputInfo.getScriptWitness(utxo.inputInfo)) match {
              case (Some(redeemScript), Some(scriptWitness)) =>
                partUpdatedPsbt
                  .addRedeemOrWitnessScriptToInput(redeemScript, index)
                  .addScriptWitnessToInput(scriptWitness, index)
              case (Some(redeemScript), None) =>
                partUpdatedPsbt
                  .addRedeemOrWitnessScriptToInput(redeemScript, index)
              case (None, Some(scriptWitness)) =>
                partUpdatedPsbt
                  .addScriptWitnessToInput(scriptWitness, index)
              case (None, None) =>
                partUpdatedPsbt
            }

        }
        assert(updatedPSBT == fullPsbt)
    })
  }

  it must "correctly construct and sign a PSBT" in {
    forAllAsync(PSBTGenerators.psbtToBeSigned) { psbtWithBuilderF =>
      psbtWithBuilderF.flatMap {
        case (psbtNoSigs, utxos) =>
          val infos = utxos.toVector.zipWithIndex.map {
            case (utxo: ScriptSignatureParams[InputInfo], index) =>
              (index, utxo)
          }
          val signedPSBTF = infos.foldLeft(Future.successful(psbtNoSigs)) {
            case (unsignedPSBTF, (index, info)) =>
              unsignedPSBTF.flatMap { unsignedPSBT =>
                info.toSingles.foldLeft(Future.successful(unsignedPSBT)) {
                  (psbtToSignF, singleInfo) =>
                    psbtToSignF.flatMap(
                      _.sign(index,
                             singleInfo.signer,
                             singleInfo.conditionalPath))
                }
              }
          }
          signedPSBTF.map { signedPSBT =>
            val finalizedPsbtT = signedPSBT.finalizePSBT
            finalizedPsbtT match {
              case Success(finalizedPsbt) =>
                assert(finalizedPsbt.extractTransactionAndValidate.isSuccess)
              case Failure(exception) => fail(exception)
            }
          }
      }
    }
  }

  it must "add Redeem Scripts to outputs" in {
    forAllAsync(PSBTGenerators.psbtWithBuilderAndP2SHOutputs(finalized = false)) {
      psbtWithBuilderF =>
        psbtWithBuilderF.flatMap {
          case (psbtEmptyOutputs, _, redeemScripts) =>
            val psbtWithOutputs = redeemScripts.zipWithIndex.foldLeft(
              psbtEmptyOutputs)((psbt, spk) =>
              psbt.addRedeemOrWitnessScriptToOutput(spk._1, spk._2))

            val allOutputsValid =
              psbtWithOutputs.outputMaps.zip(redeemScripts).forall {
                case (map, spk) =>
                  map.redeemScriptOpt.contains(RedeemScript(spk))
              }
            assert(allOutputsValid)
        }
    }
  }

  it must "add Witness Scripts to outputs" in {
    forAllAsync(
      PSBTGenerators.psbtWithBuilderAndP2WSHOutputs(finalized = false)) {
      psbtWithBuilderF =>
        psbtWithBuilderF.flatMap {
          case (psbtEmptyOutputs, _, redeemScripts) =>
            val psbtWithOutputs = redeemScripts.zipWithIndex.foldLeft(
              psbtEmptyOutputs)((psbt, spk) =>
              psbt.addRedeemOrWitnessScriptToOutput(spk._1, spk._2))

            val allOutputsValid =
              psbtWithOutputs.outputMaps.zip(redeemScripts).forall {
                case (map, spk) =>
                  map.witnessScriptOpt.contains(WitnessScript(spk))

              }
            assert(allOutputsValid)
        }
    }
  }

  it must "correctly construct and finalize PSBTs from UTXOSpendingInfo" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs(),
                ScriptGenerators.scriptPubKey,
                ChainParamsGenerator.bitcoinNetworkParams) {
      case ((creditingTxsInfo, destinations), (changeSPK, _), network) =>
        val crediting =
          creditingTxsInfo.foldLeft(0L)(_ + _.amount.satoshis.toLong)
        val spending = destinations.foldLeft(0L)(_ + _.value.satoshis.toLong)
        val maxFee = crediting - spending
        val fee = GenUtil.sample(CurrencyUnitGenerator.feeUnit(maxFee))
        for {
          (psbt, _) <- PSBTGenerators.psbtAndBuilderFromInputs(
            finalized = false,
            creditingTxsInfo = creditingTxsInfo,
            destinations = destinations,
            changeSPK = changeSPK,
            network = network,
            fee = fee)
          (expected, _) <- PSBTGenerators.psbtAndBuilderFromInputs(
            finalized = true,
            creditingTxsInfo = creditingTxsInfo,
            destinations = destinations,
            changeSPK = changeSPK,
            network = network,
            fee = fee)
        } yield {
          val finalizedPsbtOpt = psbt.finalizePSBT
          assert(finalizedPsbtOpt.isSuccess, psbt.hex)
          assert(finalizedPsbtOpt.get == expected)
        }
    }
  }

  it must "agree with TxBuilder.sign given UTXOSpendingInfos" in {
    forAllAsync(PSBTGenerators.finalizedPSBTWithBuilder) { psbtAndBuilderF =>
      for {
        (psbt, builder) <- psbtAndBuilderF
        signedTx <- builder.sign
      } yield {
        val txT = psbt.extractTransactionAndValidate
        assert(txT.isSuccess, txT.failed)

        assert(txT.get == signedTx)
      }
    }
  }
}
