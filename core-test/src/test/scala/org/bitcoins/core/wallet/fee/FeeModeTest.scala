package org.bitcoins.core.wallet.fee

import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey
}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2PKHScriptPubKey}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.builder.{BitcoinTxBuilder, OwnedTxData}
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  P2PKHSpendingInfo
}
import org.bitcoins.testkit.core.gen.{
  CurrencyUnitGenerator,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen
import org.scalatest.Assertion

import scala.concurrent.Future

class FeeModeTest extends BitcoinSAsyncTest {
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = {
    generatorDrivenConfigNewCode
  }

  behavior of "FeeMode"

  // Can't use TransactionGenerators.realisiticOutputs as that can return List.empty
  val nonEmptyRealisticOutputsGen: Gen[List[TransactionOutput]] = Gen
    .choose(1, 5)
    .flatMap(n => Gen.listOfN(n, TransactionGenerators.realisticOutput))
    .suchThat(_.nonEmpty)

  // TODO: What about if someone is getting paid?
  /* Example:
   *           all four inputs are of value 25
   *           output 0: 35
   *           outputs 1-3: 5
   *
   * This currently causes problems since owner 0 cannot be charged a fee
   */
  def executeTest(
      feeMode: FeeMode,
      outputs: List[TransactionOutput],
      feeRate: FeeUnit)(
      assertion: (Transaction, List[CurrencyUnit]) => Assertion): Future[
    Assertion] = {
    val maxInput = outputs
      .reduce[TransactionOutput] {
        case (a, b) =>
          if (a.value > b.value) a else b
      }
      .value
      .satoshis
      .toLong

    val inputKey = ECPrivateKey.freshPrivateKey
    val perOwnerAmt = Satoshis(maxInput * 2)
    val utxos = outputs.indices.map { owner =>
      P2PKHSpendingInfo(
        outPoint =
          TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32(owner)),
        amount = perOwnerAmt,
        scriptPubKey = P2PKHScriptPubKey(inputKey.publicKey),
        signer = inputKey,
        hashType = HashType.sigHashAll
      )
    }
    val ownedUtxos: Vector[OwnedTxData[BitcoinUTXOSpendingInfo]] =
      utxos.zipWithIndex.toVector.map {
        case (utxo, owner) => OwnedTxData(utxo, owner)
      }
    val changePubKeys = outputs.indices.map(_ => ECPublicKey.freshPublicKey)
    val changeSPK = outputs.indices
      .map(owner => owner -> P2PKHScriptPubKey(changePubKeys(owner)))
      .toMap
    val network: BitcoinNetwork = RegTest

    val ownedOutputs = outputs.zipWithIndex.toVector.map {
      case (output, index) => OwnedTxData(output, index)
    }

    val txBuilder =
      BitcoinTxBuilder(ownedOutputs,
                       ownedUtxos,
                       feeRate,
                       changeSPK,
                       network,
                       feeMode)

    txBuilder.sign.map { tx =>
      val changeOutputs = tx.outputs.filterNot(outputs.contains)
      val noFeeChanges = outputs.map(output => perOwnerAmt - output.value)
      val withFeeChanges = changeOutputs.map { changeOutput =>
        val owner =
          changeSPK.find(_._2 == changeOutput.scriptPubKey).get._1
        owner -> changeOutput.value
      }.toMap
      val fees = noFeeChanges.zipWithIndex.map {
        case (noFeeChange, owner) =>
          noFeeChange - withFeeChanges(owner)
      }

      assertion(tx, fees)
    }
  }

  it should "correctly subtract fees evenly amongst outputs" in {
    forAllAsync(nonEmptyRealisticOutputsGen, CurrencyUnitGenerator.smallFeeUnit) {
      case (outputs, feeRate) =>
        executeTest(FeeMode.EqualDistribution, outputs, feeRate) { (_, fees) =>
          val firstFee = fees.head.satoshis.toLong
          assert(
            fees.forall(fee => Math.abs(fee.satoshis.toLong - firstFee) <= 1L))
        }
    }
  }

  def closeTogether(
      num1: Double,
      num2: Double,
      percision: Double = 1.0e-6): Boolean = {
    Math.abs(num1 - num2) < percision
  }

  it should "correctly subtract fees proportionally amongst outputs" in {
    forAllAsync(nonEmptyRealisticOutputsGen, CurrencyUnitGenerator.smallFeeUnit) {
      case (outputs, feeRate) =>
        executeTest(FeeMode.DistributionByOutputValue, outputs, feeRate) {
          (_, fees) =>
            val firstFee = fees.head.satoshis.toLong
            val firstSize = outputs.head.value.satoshis.toLong
            val firstFeeProportion = firstFee.toDouble / firstSize

            // Fee has been proportionally distributed by output (up to some small remainder)
            val closeProportions = fees.zip(outputs).forall {
              case (fee, output) =>
                val feeProportion = fee.satoshis.toLong.toDouble / output.value.satoshis.toLong

                closeTogether(feeProportion / firstFeeProportion, 1)
            }

            assert(closeProportions)
        }
    }
  }

  // TODO: This doesn't actually test anything yet, need a generator for owned tx stuff
  it should "correctly subtract fees proportionally amongst inputs" in {
    forAllAsync(nonEmptyRealisticOutputsGen, CurrencyUnitGenerator.smallFeeUnit) {
      case (outputs, feeRate) =>
        executeTest(FeeMode.DistributionByInputSize, outputs, feeRate) {
          (tx, fees) =>
            val firstFee = fees.head.satoshis.toLong
            val firstSize = tx.inputs.head.bytes.size
            val firstFeeProportion = firstFee.toDouble / firstSize

            // Fee has been proportionally distributed by output (up to some small remainder)
            val closeProportions = fees.zip(tx.inputs).forall {
              case (fee, input) =>
                val feeProportion = fee.satoshis.toLong.toDouble / input.bytes.size

                closeTogether(feeProportion / firstFeeProportion, 1, 0.05)
            }

            assert(closeProportions)
        }
    }
  }
}
