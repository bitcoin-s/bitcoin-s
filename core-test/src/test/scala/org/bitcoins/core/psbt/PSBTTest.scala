package org.bitcoins.core.psbt

import org.bitcoins.core.psbt.InputPSBTRecord.{
  HASH160PreImage,
  HASH256PreImage,
  RIPEMD160PreImage,
  SHA256PreImage
}
import org.bitcoins.core.psbt.OutputPSBTRecord.{RedeemScript, WitnessScript}
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.testkitcore.gen._
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import scodec.bits._

import scala.util.{Failure, Success}

class PSBTTest extends BitcoinSUnitTest {

  behavior of "PSBT"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it must "correctly combine PSBTs" in {
    forAll(PSBTGenerators.arbitraryPSBT) { psbt =>
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
      val oppositeInputs = inputs.zip(newInputs).map { case (map, pruned) =>
        InputPSBTMap(map.elements.filterNot(e => pruned.elements.contains(e)))
      }
      val oppositeOutputs =
        outputs.zip(newOutputs).map { case (map, pruned) =>
          OutputPSBTMap(
            map.elements.filterNot(e => pruned.elements.contains(e)))
        }

      val psbt2 = PSBT(oppositeGlobal, oppositeInputs, oppositeOutputs)

      assert(psbt1.combinePSBT(psbt2) == psbt)
      assert(psbt2.combinePSBT(psbt1) == psbt)
    }
  }

  it must "correctly update PSBTs' inputs" in {
    forAll(PSBTGenerators.psbtToBeSigned) { case (fullPsbt, utxos, _) =>
      val emptyPsbt = PSBT.fromUnsignedTx(fullPsbt.transaction)

      val infoAndTxs =
        PSBTGenerators.orderSpendingInfos(fullPsbt.transaction, utxos.toVector)
      val updatedPSBT =
        infoAndTxs.zipWithIndex
          .foldLeft(emptyPsbt) { case (psbt, (utxo, index)) =>
            val partUpdatedPsbt = psbt
              .addUTXOToInput(utxo.prevTransaction, index)
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
    }
  }

  it must "correctly construct and sign a PSBT" in {
    forAll(PSBTGenerators.psbtToBeSigned) { case (psbtNoSigs, utxos, _) =>
      val infos = utxos.toVector.zipWithIndex.map {
        case (utxo: ScriptSignatureParams[InputInfo], index) =>
          (index, utxo)
      }
      val signedPSBT = infos.foldLeft(psbtNoSigs) {
        case (unsignedPSBT, (index, info)) =>
          info.toSingles.foldLeft(unsignedPSBT) { (psbtToSign, singleInfo) =>
            psbtToSign.sign(index,
                            singleInfo.signer,
                            singleInfo.conditionalPath)
          }
      }
      val finalizedPsbtT = signedPSBT.finalizePSBT
      finalizedPsbtT match {
        case Success(finalizedPsbt) =>
          val txT = finalizedPsbt.extractTransactionAndValidate
          assert(txT.isSuccess, txT.failed)
        case Failure(exception) => fail(exception)
      }
    }
  }

  it must "add Redeem Scripts to outputs" in {
    forAll(PSBTGenerators.psbtWithBuilderAndP2SHOutputs(finalized = false)) {
      case (psbtEmptyOutputs, _, redeemScripts) =>
        val psbtWithOutputs =
          redeemScripts.zipWithIndex.foldLeft(psbtEmptyOutputs)((psbt, spk) =>
            psbt.addRedeemOrWitnessScriptToOutput(spk._1, spk._2))

        val allOutputsValid =
          psbtWithOutputs.outputMaps.zip(redeemScripts).forall {
            case (map, spk) =>
              map.redeemScriptOpt.contains(RedeemScript(spk))
          }
        assert(allOutputsValid)
    }
  }

  it must "add Witness Scripts to outputs" in {
    forAll(PSBTGenerators.psbtWithBuilderAndP2WSHOutputs(finalized = false)) {
      case (psbtEmptyOutputs, _, redeemScripts) =>
        val psbtWithOutputs =
          redeemScripts.zipWithIndex.foldLeft(psbtEmptyOutputs)((psbt, spk) =>
            psbt.addRedeemOrWitnessScriptToOutput(spk._1, spk._2))

        val allOutputsValid =
          psbtWithOutputs.outputMaps.zip(redeemScripts).forall {
            case (map, spk) =>
              map.witnessScriptOpt.contains(WitnessScript(spk))

          }
        assert(allOutputsValid)
    }
  }

  it must "correctly construct and finalize PSBTs from UTXOSpendingInfo" in {
    forAll(CreditingTxGen.inputsAndOutputs(),
           ScriptGenerators.scriptPubKey,
           ChainParamsGenerator.bitcoinNetworkParams) {
      case ((creditingTxsInfo, destinations), (changeSPK, _), _) =>
        val crediting =
          creditingTxsInfo.foldLeft(0L)(_ + _.amount.satoshis.toLong)
        val spending = destinations.foldLeft(0L)(_ + _.value.satoshis.toLong)
        val maxFee = crediting - spending
        val fee = GenUtil.sample(FeeUnitGen.feeUnit(maxFee))

        val (psbt, _, _) =
          PSBTGenerators.psbtAndBuilderFromInputs(finalized = false,
                                                  creditingTxsInfo =
                                                    creditingTxsInfo,
                                                  destinations = destinations,
                                                  changeSPK = changeSPK,
                                                  fee = fee)
        val (expected, _, _) =
          PSBTGenerators.psbtAndBuilderFromInputs(finalized = true,
                                                  creditingTxsInfo =
                                                    creditingTxsInfo,
                                                  destinations = destinations,
                                                  changeSPK = changeSPK,
                                                  fee = fee)

        val finalizedPsbtOpt = psbt.finalizePSBT
        assert(finalizedPsbtOpt.isSuccess, psbt.hex)
        assert(finalizedPsbtOpt.get == expected)
    }
  }

  it must "agree with TxBuilder.sign given UTXOSpendingInfos" in {
    forAll(PSBTGenerators.finalizedPSBTWithBuilder) {
      case (psbt, builder, fee) =>
        val signedTx = builder.sign(fee)

        val txT = psbt.extractTransactionAndValidate
        assert(txT.isSuccess, txT.failed)

        assert(txT.get == signedTx)
    }
  }

  it must "correctly serialize RIPEMD160 hash preimages" in {
    forAll(StringGenerators.hexString) { hex =>
      val preimage = ByteVector.fromValidHex(hex)

      val ripeMd160 = RIPEMD160PreImage(preimage)
      val ripeMd160Hash = CryptoUtil.ripeMd160(preimage)
      assert(ripeMd160.hash == ripeMd160Hash)
      assert(ripeMd160.value == preimage)
      assert(ripeMd160.key == hex"0a" ++ ripeMd160Hash.bytes)
    }
  }

  it must "correctly serialize SHA256 hash preimages" in {
    forAll(StringGenerators.hexString) { hex =>
      val preimage = ByteVector.fromValidHex(hex)

      val sha256 = SHA256PreImage(preimage)
      val sha256Hash = CryptoUtil.sha256(preimage)
      assert(sha256.hash == sha256Hash)
      assert(sha256.value == preimage)
      assert(sha256.key == hex"0b" ++ sha256Hash.bytes)
    }
  }

  it must "correctly serialize HASH160 hash preimages" in {
    forAll(StringGenerators.hexString) { hex =>
      val preimage = ByteVector.fromValidHex(hex)

      val hash160 = HASH160PreImage(preimage)
      val hash160Hash = CryptoUtil.sha256Hash160(preimage)
      assert(hash160.hash == hash160Hash)
      assert(hash160.value == preimage)
      assert(hash160.key == hex"0c" ++ hash160Hash.bytes)
    }
  }

  it must "correctly serialize HASH256 hash preimages" in {
    forAll(StringGenerators.hexString) { hex =>
      val preimage = ByteVector.fromValidHex(hex)

      val hash256 = HASH256PreImage(preimage)
      val hash256Hash = CryptoUtil.doubleSHA256(preimage)
      assert(hash256.hash == hash256Hash)
      assert(hash256.value == preimage)
      assert(hash256.key == hex"0d" ++ hash256Hash.bytes)
    }
  }
}
